{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Cardano.Kuber.Util
(
localNodeConnInfo


    -- TypeCast/Conversion Utilities (Address/Pkh/SignKey)
    , skeyToAddr
    , skeyToAddrInEra
    , sKeyToPkh
    , addressInEraToAddressAny
    , pkhToMaybeAddr
    , addrToMaybePkh
    , addrInEraToPkh
    , addressInEraToPaymentKeyHash

    -- TypeCast/Conversion Utilities (PlutusTypes)
    , dataToScriptData
    , toPlutusAssetClass
 
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
    , calculateTxoutMinLovelaceWithcpw
    , calculateTxoutMinLovelaceFunc
    , calculateTxoutMinLovelace
    , evaluateExecutionUnits
    , evaluateExUnitMap

    -- query helpers
    , queryUtxos



)
where

import Cardano.Api
import Cardano.Kuber.Utility.DataTransformation
import Data.ByteString (ByteString,readFile)
import qualified Cardano.Api.Shelley as Shelley
import qualified Data.Set as Set
import Control.Exception (try, throw)
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Cardano.Kuber.Error ( FrameworkError(FrameworkError), ErrorType(WrongScriptType, ExUnitCalculationError, FeatureNotSupported, PlutusScriptError) )
import Plutus.V1.Ledger.Api (fromBuiltin, toBuiltin, ToData, toData, CurrencySymbol (CurrencySymbol), TokenName (TokenName), PubKeyHash (PubKeyHash), Address)
import System.FilePath (joinPath)
import Cardano.Api.Shelley (ProtocolParameters (protocolParamUTxOCostPerWord), fromPlutusData, TxBody (ShelleyTxBody), Lovelace (Lovelace), toShelleyTxOut, Address (ShelleyAddress), fromShelleyStakeCredential, fromShelleyStakeReference, fromShelleyAddr, toShelleyAddr, fromShelleyPaymentCredential, fromShelleyTxIn)
import qualified Cardano.Ledger.Alonzo.Tx as LedgerBody
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess, SubmitFail))
import Data.Text.Conversions (convertText, Base16 (unBase16, Base16), FromText (fromText), ToText (toText))
import Data.Functor ((<&>))

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Text.Encoding
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text.IO as TextIO
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import Plutus.V1.Ledger.Value (AssetClass(AssetClass))
import Data.String (fromString)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
-- import Shelley.Spec.Ledger.API (Credential(ScriptHashObj, KeyHashObj), KeyHash (KeyHash), StakeReference (StakeRefNull))
import Codec.Serialise (serialise)
import Cardano.Api.Byron (Address(ByronAddress))
import qualified Data.Aeson as JSON
import qualified Data.Text.Encoding as TSE
import Cardano.Kuber.Data.Parsers
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Set (Set)
import Cardano.Ledger.Shelley.API (Credential(ScriptHashObj, KeyHashObj), KeyHash (KeyHash), StakeReference (StakeRefNull))
import Data.Map (Map)
import qualified Codec.CBOR.Write as Cborg
import qualified Codec.CBOR.Encoding as Cborg
import qualified Cardano.Binary as Cborg
import qualified Data.ByteString as BS
import Cardano.Slotting.Time (SystemStart)
import Cardano.Kuber.Core.ChainInfo
import Cardano.Kuber.Utility.QueryHelper
import Cardano.Ledger.Alonzo.TxBody (inputs')
import Cardano.Ledger.Shelley.UTxO (txins)



localNodeConnInfo :: NetworkId -> FilePath   -> LocalNodeConnectInfo CardanoMode
localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))






calculateTxoutMinLovelace :: TxOut CtxUTxO  AlonzoEra -> ProtocolParameters -> Maybe Lovelace
calculateTxoutMinLovelace txout pParams=do
  costPerWord <- protocolParamUTxOCostPerWord pParams
  pure $calculateTxoutMinLovelaceWithcpw costPerWord txout 

calculateTxoutMinLovelaceFunc :: ProtocolParameters  -> Maybe ( TxOut CtxUTxO    AlonzoEra -> Lovelace)
calculateTxoutMinLovelaceFunc pParams = do
  costPerWord <- protocolParamUTxOCostPerWord pParams
  pure $ calculateTxoutMinLovelaceWithcpw costPerWord

calculateTxoutMinLovelaceWithcpw :: Lovelace -> TxOut CtxUTxO  AlonzoEra -> Lovelace  
calculateTxoutMinLovelaceWithcpw (Lovelace cpw) txout = Lovelace  $ Alonzo.utxoEntrySize (toShelleyTxOut ShelleyBasedEraAlonzo   txout) * cpw



isNullValue :: Value -> Bool
isNullValue v = not $ any (\(aid,Quantity q) -> q>0) (valueToList v)

isPositiveValue :: Value -> Bool
isPositiveValue v = not $ any (\(aid,Quantity q) -> q<0) (valueToList v)

filterNegativeQuantity :: Value -> [(AssetId,Quantity)]
filterNegativeQuantity  v = filter (\(_, v) -> v < 0 ) $ valueToList v


txoutListSum :: [TxOut ctx era ] -> Value
txoutListSum = foldMap toValue
  where
    toValue (TxOut _ val _)= case val of
      TxOutValue masie va -> va

utxoListSum :: [(a, TxOut ctx era)] -> Value
utxoListSum l = txoutListSum (map snd l)

utxoMapSum :: Map a (TxOut ctx era) -> Value
utxoMapSum x = txoutListSum  $ Map.elems x

utxoSum :: UTxO AlonzoEra  -> Value
utxoSum (UTxO uMap)= utxoMapSum uMap


evaluateExecutionUnits :: DetailedChainInfo ->  Tx AlonzoEra -> IO [Either String ExecutionUnits]
evaluateExecutionUnits (DetailedChainInfo costPerWord conn pParam systemStart eraHistory)  tx = do 
      let txbody =  getTxBody tx

          inputs :: Set.Set TxIn 
          inputs = case txbody of {ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn   $ inputs' tb }
      txins <- queryTxins  conn  inputs 
      case txins of 
        Left fe -> throw $ FrameworkError  ExUnitCalculationError  (show fe)
        Right uto -> case evaluateTransactionExecutionUnits AlonzoEraInCardanoMode systemStart eraHistory pParam uto txbody of 
            Left tve -> throw $ FrameworkError ExUnitCalculationError $ show tve
            Right exUnitMap -> pure $ Prelude.map (\case
                                        Left see -> Left (show see)
                                        Right eu -> Right eu  )   (Map.elems exUnitMap)



evaluateExUnitMapIO ::  DetailedChainInfo  ->  TxBody AlonzoEra -> IO ( Either FrameworkError   (Map TxIn ExecutionUnits))
evaluateExUnitMapIO dcinfo txbody = do 
  let 
      inputs :: Set.Set TxIn 
      inputs = case txbody of {ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn   $ inputs' tb }
  txins <- queryTxins  (dciConn dcinfo)  inputs 
  case txins of 
    Left fe -> throw $ FrameworkError  ExUnitCalculationError  (show fe)
    Right uto ->  pure $  evaluateExUnitMap dcinfo uto txbody 

evaluateExUnitMap :: DetailedChainInfo  -> UTxO AlonzoEra ->  TxBody AlonzoEra ->  Either FrameworkError   (Map TxIn ExecutionUnits)
evaluateExUnitMap  (DetailedChainInfo cpw conn pParam systemStart eraHistory ) usedUtxos txbody   = case eExUnits of
  Left tve -> Left $ FrameworkError   ExUnitCalculationError (show tve)
  Right map ->mapM  doMap ( Map.toList map) <&> Map.fromList

  where
    eExUnits=evaluateTransactionExecutionUnits AlonzoEraInCardanoMode systemStart eraHistory pParam usedUtxos txbody
    inputList=case txbody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv ->  Set.toList (txins tb) }
    inputLookup = Map.fromAscList $ zip [0..] inputList

    doMap :: (ScriptWitnessIndex,Either ScriptExecutionError ExecutionUnits) -> Either FrameworkError  (TxIn,ExecutionUnits)
    doMap  (i,mExUnitResult)= case i of
      ScriptWitnessIndexTxIn wo -> unEitherExUnits (fromShelleyTxIn (inputList !! fromIntegral  wo),) mExUnitResult
      ScriptWitnessIndexMint wo -> Left  $ FrameworkError FeatureNotSupported "Automatically calculating ex units for mint is not supported"
      ScriptWitnessIndexCertificate wo ->  Left  $ FrameworkError FeatureNotSupported "Witness for Certificates is not supported"
      ScriptWitnessIndexWithdrawal wo ->  Left  $ FrameworkError FeatureNotSupported "Plutus script for withdrawl is not supported"

    unEitherExUnits :: (ExecutionUnits -> b) ->  Either ScriptExecutionError ExecutionUnits ->  Either FrameworkError  b
    unEitherExUnits f v= case v of
      Right e -> Right $ f e
      Left e -> case e of
        ScriptErrorEvaluationFailed ee txts -> Left (FrameworkError PlutusScriptError  (T.unpack $ T.intercalate (T.pack ", ") txts ))
        _  -> Left (FrameworkError ExUnitCalculationError (show e))


    transformIn (txIn,wit) exUnit= (txIn  ,case BuildTxWith $ KeyWitness KeyWitnessForSpending of {
      BuildTxWith wit' -> wit } )


valueLte :: Value -> Value -> Bool
valueLte _v1 _v2= not $ any (\(aid,Quantity q) -> q > lookup aid) (valueToList _v1) -- do we find anything that's greater than q
  where
    lookup x= case Map.lookup x v2Map of
      Nothing -> 0
      Just (Quantity v) -> v
    v2Map=Map.fromList $ valueToList _v2