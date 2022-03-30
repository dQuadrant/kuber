{-# LANGUAGE NamedFieldPuns #-}
module Cardano.Contrib.Easy.TxFramework where


import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Contrib.Easy.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Contrib.Easy.Util
import Data.Functor ((<&>))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)
import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript)
import Cardano.Contrib.Easy.TxBuilder
import Cardano.Contrib.Easy.ChainInfo (DetailedChainInfo, ChainInfo)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)


mkTx ::  TxContext  -> Either String (TxBody AlonzoEra)
mkTx = error "sad"

-- construct txBody using given inputs and the txBuilder and the change value
txBuilderToTxBody extraInputs fee change (TxBuilder selections inputs outputs mintingScripts collaterals validityStart validityEnd mintValue extraSignatures explicitFee defaultChangeAddr ) =
  error "sad"
  where
    resoveInputs :: MonadFail m => TxInput -> m TxInputResolved_
    resoveInputs v = case v of
      TxInputResolved tir -> pure tir
      TxInputUnResolved tiur -> do

mkTxExplicitFee ::DetailedChainInfo -> TxBuilder -> TxBody AlonzoEra
mkTxExplicitFee = error "sad"

gatherInfo :: ChainInfo i -> i  -> TxBuilder  ->  IO (Either AcquireFailure TxContext)
gatherInfo cInfo  txBuilder@TxBuilder{txSelections, txInputs} = do


