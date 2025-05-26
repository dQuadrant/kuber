module Cardano.Kuber.Util
  ( -- TypeCast/Conversion Utilities (Address/Pkh/SignKey)
    skeyToAddr,
    skeyToAddrInEra,
    sKeyToPkh,
    fromLedgerAddress,
    addressInEraToAddressAny,
    pkhToMaybeAddr,
    addrToMaybePkh,
    addrInEraToPkh,
    addressInEraToPaymentKeyHash,
    addressNetworkId,
    -- TypeCast/Conversion Utilities (PlutusTypes)
    dataToScriptData,
    toPlutusAssetClass,                                                                    
    fromPlutusData,
    fromPlutusAddress,
    toPlutusAddress,
    toPlutusCredential,
    plutusAssetClassToAssetId,
    addrInEraToPlutusAddress,
    addressToPlutusCredential,
    fromPlutusV1Script,
    fromPlutusValue,
    toPlutusValue,
    fromPlutusV2Script,
    toPlutusTxOutRef,
    -- Value utility and utxoto Value
    isNullValue,
    valueLte,
    isPositiveValue,
    filterNegativeQuantity,
    utxoListSum,
    utxoSum,
    utxoMapSum,
    txoutListSum,
    -- calculation functions
    calculateTxoutMinLovelaceOrErr,
    calculateTxoutMinLovelace,
    evaluateFee,
    txoutMinLovelace,
    -- query helpers
    queryUtxos,
    queryTxins,
    queryAddressInEraUtxos,
    -- metadata utility
    splitMetadataStrings,
    -- wallet utilities
    readSignKey,
    getDefaultSignKey,
    -- text utilities
    toHexString,
    unHex,
    unHexLazy,
    getDefaultConnection,
    -- time conversion utility
    timestampToSlot,
    slotToTimestamp,
  )
where

import Cardano.Api
-- import Shelley.Spec.Ledger.API (Credential(ScriptHashObj, KeyHashObj), KeyHash (KeyHash), StakeReference (StakeRefNull))

import Cardano.Api.Shelley (Address (ShelleyAddress), TxBody (ShelleyTxBody), fromPlutusData, fromShelleyAddr, fromShelleyPaymentCredential, fromShelleyScriptHash, fromShelleyStakeCredential, fromShelleyStakeReference, fromShelleyTxIn, toShelleyAddr, toShelleyTxOut)
import qualified Cardano.Api.Shelley as Shelley
import qualified Cardano.Binary as Cborg
import Cardano.Kuber.Core.ChainInfo
import Cardano.Kuber.Error (ErrorType (ExUnitCalculationError, FeatureNotSupported, PlutusScriptError, WrongScriptType), FrameworkError (FrameworkError))
import Cardano.Kuber.Utility.ChainInfoUtil
import Cardano.Kuber.Utility.DataTransformation
-- import qualified Cardano.Ledger.Mary.Value as Ledger

import Cardano.Kuber.Utility.Misc
import Cardano.Kuber.Utility.QueryHelper
import Cardano.Kuber.Utility.ScriptUtil (fromPlutusV1Script, fromPlutusV2Script)
import Cardano.Kuber.Utility.Text
import Cardano.Kuber.Utility.WalletUtil (getDefaultSignKey, readSignKey)
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as LedgerBody
import Cardano.Ledger.Babbage.TxBody (btbInputs, mint')
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (Credential (KeyHashObj, ScriptHashObj), KeyHash (KeyHash), StakeReference (StakeRefNull))
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import Cardano.Ledger.Shelley.UTxO (txins)
import Cardano.Slotting.Time (RelativeTime, SystemStart, fromRelativeTime, toRelativeTime)
import qualified Codec.CBOR.Encoding as Cborg
import qualified Codec.CBOR.Write as Cborg
import Codec.Serialise (serialise)
import Control.Exception (throw, try)
import qualified Data.Aeson as A
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as A
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Builder (charUtf8)
import qualified Data.ByteString.Builder as BSL
import qualified Data.ByteString.Builder as Builder
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Char (toLower)
import qualified Data.Char as C
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Conversions (Base16 (Base16, unBase16), FromText (fromText), ToText (toText), convertText)
import Data.Text.Encoding
import qualified Data.Text.Encoding as TSE
import qualified Data.Text.IO as TextIO
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import qualified Debug.Trace as Debug
import Ouroboros.Consensus.HardFork.History (unsafeExtendSafeZone)
import qualified Ouroboros.Consensus.HardFork.History as Qry
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (SubmitFail, SubmitSuccess))
import PlutusLedgerApi.V2 (Address, CurrencySymbol (CurrencySymbol), PubKeyHash (PubKeyHash), ToData, TokenName (TokenName), fromBuiltin, toBuiltin, toData)
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath (joinPath)
