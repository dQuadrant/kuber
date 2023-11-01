module Cardano.Kuber.Util
(

    -- TypeCast/Conversion Utilities (Address/Pkh/SignKey)
      skeyToAddr
    , skeyToAddrInEra
    , sKeyToPkh
    , addressInEraToAddressAny
    , pkhToMaybeAddr
    , addrToMaybePkh
    , addrInEraToPkh
    , addressInEraToPaymentKeyHash
    , addressNetworkId

    -- TypeCast/Conversion Utilities (PlutusTypes)
    , dataToScriptData
    , toPlutusAssetClass
    , fromPlutusData
    , fromPlutusAddress
    , toPlutusAddress
    , toPlutusCredential
    , addrInEraToPlutusAddress
    , addressToPlutusCredential
    , fromPlutusV1Script
    , fromPlutusV2Script

    -- Value utility and utxoto Value
    , isNullValue
    , valueLte
    , isPositiveValue
    , filterNegativeQuantity
    , utxoListSum
    , utxoSum
    , utxoMapSum
    , txoutListSum

    -- calculation functions
    , calculateTxoutMinLovelaceOrErr
    , calculateTxoutMinLovelace
    , evaluateFee
    , babbageMinLovelace

    -- query helpers
    , queryUtxos
    , queryTxins
    , queryAddressInEraUtxos

    -- metadata utility
    , splitMetadataStrings

    -- wallet utilities
    , readSignKey
    , getDefaultSignKey

    -- text utilities
    , toHexString
    , unHex
    , unHexLazy
    , getDefaultConnection

  -- time conversion utility
    , timestampToSlot
    , slotToTimestamp
)
where

import Cardano.Api
import Cardano.Kuber.Utility.DataTransformation
import Cardano.Kuber.Core.ChainInfo
import Cardano.Kuber.Utility.QueryHelper

import qualified Cardano.Api.Shelley as Shelley
import qualified Data.Set as Set
import Control.Exception (try, throw)
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Cardano.Kuber.Error ( FrameworkError(FrameworkError), ErrorType(WrongScriptType, ExUnitCalculationError, FeatureNotSupported, PlutusScriptError) )
import PlutusLedgerApi.V2 (fromBuiltin, toBuiltin, ToData, toData, CurrencySymbol (CurrencySymbol), TokenName (TokenName), PubKeyHash (PubKeyHash), Address)
import System.FilePath (joinPath)
import Cardano.Api.Shelley ( fromPlutusData, TxBody (ShelleyTxBody), Lovelace (Lovelace), toShelleyTxOut, Address (ShelleyAddress), fromShelleyStakeCredential, fromShelleyStakeReference, fromShelleyAddr, toShelleyAddr, fromShelleyPaymentCredential, fromShelleyTxIn, fromShelleyScriptHash)
import qualified Cardano.Ledger.Babbage.Tx as LedgerBody
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess, SubmitFail))
import Data.Text.Conversions (convertText, Base16 (unBase16, Base16), FromText (fromText), ToText (toText))
import Data.Functor ((<&>))

import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text.IO as TextIO
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley

import Data.String (fromString)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
-- import Shelley.Spec.Ledger.API (Credential(ScriptHashObj, KeyHashObj), KeyHash (KeyHash), StakeReference (StakeRefNull))
import Codec.Serialise (serialise)
import Cardano.Api.Byron (Address(ByronAddress))
import qualified Data.Aeson as JSON
import qualified Data.Text.Encoding as TSE
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Set (Set)
import Cardano.Ledger.Shelley.API (Credential(ScriptHashObj, KeyHashObj), KeyHash (KeyHash), StakeReference (StakeRefNull))
import Data.Map (Map)
import qualified Codec.CBOR.Write as Cborg
import qualified Codec.CBOR.Encoding as Cborg
import qualified Cardano.Binary as Cborg
import Cardano.Slotting.Time (SystemStart, RelativeTime, toRelativeTime, fromRelativeTime)
import Cardano.Ledger.Babbage.TxBody (btbInputs, mint')
import Cardano.Ledger.Shelley.UTxO (txins)
import Cardano.Kuber.Utility.ChainInfoUtil
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import Data.Int
import qualified Data.Char as C
import Data.Word (Word64)

import Data.ByteString ( readFile, ByteString )
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSL
import Data.ByteString.Builder (charUtf8)
import Cardano.Kuber.Utility.WalletUtil (readSignKey, getDefaultSignKey)
import Cardano.Kuber.Utility.Text
-- import qualified Cardano.Ledger.Mary.Value as Ledger
import Data.Either (partitionEithers)
import Data.Bifunctor (Bifunctor(bimap))
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Debug.Trace as Debug
import Data.List (intercalate)
import qualified Cardano.Ledger.Alonzo as Alonzo
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Kuber.Utility.ScriptUtil (fromPlutusV1Script, fromPlutusV2Script)
import qualified Ouroboros.Consensus.HardFork.History as Qry
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime, utcTimeToPOSIXSeconds)
import Ouroboros.Consensus.HardFork.History (unsafeExtendSafeZone)
import Cardano.Kuber.Utility.Misc




