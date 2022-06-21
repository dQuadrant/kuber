{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
    

    -- TypeCast/Conversion Utilities (PlutusTypes)
    , dataToScriptData
    , toPlutusAssetClass
    , fromPlutusData
    , fromPlutusAddress 
    , toPlutusAddress
    , toPlutusCredential
    , addrInEraToPlutusAddress
    , addressToPlutusCredential

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
    , performQuery
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
import Plutus.V1.Ledger.Api (fromBuiltin, toBuiltin, ToData, toData, CurrencySymbol (CurrencySymbol), TokenName (TokenName), PubKeyHash (PubKeyHash), Address)
import System.FilePath (joinPath)
import Cardano.Api.Shelley (ProtocolParameters (protocolParamUTxOCostPerWord), fromPlutusData, TxBody (ShelleyTxBody), Lovelace (Lovelace), toShelleyTxOut, Address (ShelleyAddress), fromShelleyStakeCredential, fromShelleyStakeReference, fromShelleyAddr, toShelleyAddr, fromShelleyPaymentCredential, fromShelleyTxIn)
import qualified Cardano.Ledger.Alonzo.Tx as LedgerBody
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess, SubmitFail))
import Data.Text.Conversions (convertText, Base16 (unBase16, Base16), FromText (fromText), ToText (toText))
import Data.Functor ((<&>))

import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text.IO as TextIO
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import qualified Cardano.Ledger.Babbage.Rules.Utxo as Babbage

import Plutus.V1.Ledger.Value (AssetClass(AssetClass))
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
import Cardano.Slotting.Time (SystemStart)
import Cardano.Ledger.Babbage.TxBody (inputs)
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




calculateTxoutMinLovelace :: TxOut CtxUTxO  BabbageEra -> ProtocolParameters -> Maybe Lovelace
calculateTxoutMinLovelace txout pParams=do
  costPerWord <- protocolParamUTxOCostPerWord pParams
  pure $calculateTxoutMinLovelaceWithcpw costPerWord txout

calculateTxoutMinLovelaceFunc :: ProtocolParameters  -> Maybe ( TxOut CtxUTxO    BabbageEra -> Lovelace)
calculateTxoutMinLovelaceFunc pParams = do
  costPerWord <- protocolParamUTxOCostPerWord pParams
  pure $ calculateTxoutMinLovelaceWithcpw costPerWord

-- TODO: fix this for babbage era.
calculateTxoutMinLovelaceWithcpw :: Lovelace -> TxOut CtxUTxO  BabbageEra -> Lovelace
calculateTxoutMinLovelaceWithcpw (Lovelace cpw) txout = Lovelace  $ Alonzo.utxoEntrySize (toShelleyTxOut ShelleyBasedEraBabbage   txout) * cpw



isNullValue :: Value -> Bool
isNullValue v = not $ any (\(aid,Quantity q) -> q>0) (valueToList v)

isPositiveValue :: Value -> Bool
isPositiveValue v = not $ any (\(aid,Quantity q) -> q<0) (valueToList v)

valueLte :: Value -> Value -> Bool
valueLte _v1 _v2= not $ any (\(aid,Quantity q) -> q > lookup aid) (valueToList _v1) -- do we find anything that's greater than q
  where
    lookup x= case Map.lookup x v2Map of
      Nothing -> 0
      Just (Quantity v) -> v
    v2Map=Map.fromList $ valueToList _v2

filterNegativeQuantity :: Value -> [(AssetId,Quantity)]
filterNegativeQuantity  v = filter (\(_, v) -> v < 0 ) $ valueToList v


txoutListSum :: [TxOut ctx era ] -> Value
txoutListSum = foldMap toValue
  where
    toValue (TxOut _ val _ _)= case val of
      TxOutValue masie va -> va

utxoListSum :: [(a, TxOut ctx era)] -> Value
utxoListSum l = txoutListSum (map snd l)

utxoMapSum :: Map a (TxOut ctx era) -> Value
utxoMapSum x = txoutListSum  $ Map.elems x

utxoSum :: UTxO BabbageEra  -> Value
utxoSum (UTxO uMap)= utxoMapSum uMap


evaluateExecutionUnits :: DetailedChainInfo ->  Tx BabbageEra -> IO [Either String ExecutionUnits]
evaluateExecutionUnits (DetailedChainInfo costPerWord conn pParam systemStart eraHistory)  tx = do
      let txbody =  getTxBody tx

          _inputs :: Set.Set TxIn
          _inputs = case txbody of {ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn   $ inputs tb }
      txins <- queryTxins  conn  _inputs
      case txins of
        Left fe -> throw $ FrameworkError  ExUnitCalculationError  (show fe)
        Right uto -> case evaluateTransactionExecutionUnits BabbageEraInCardanoMode systemStart eraHistory pParam uto txbody of
            Left tve -> throw $ FrameworkError ExUnitCalculationError $ show tve
            Right exUnitMap -> pure $ Prelude.map (\case
                                        Left see -> Left (show see)
                                        Right eu -> Right eu  )   (Map.elems exUnitMap)



evaluateExUnitMapIO ::  DetailedChainInfo  ->  TxBody BabbageEra -> IO ( Either FrameworkError   (Map TxIn ExecutionUnits))
evaluateExUnitMapIO dcinfo txbody = do
  let
      _inputs :: Set.Set TxIn
      _inputs = case txbody of {ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn   $ inputs tb }
  txins <- queryTxins  (dciConn dcinfo)  _inputs
  case txins of
    Left fe -> throw $ FrameworkError  ExUnitCalculationError  (show fe)
    Right uto ->  pure $  evaluateExUnitMap dcinfo uto txbody

evaluateExUnitMap :: DetailedChainInfo  -> UTxO BabbageEra ->  TxBody BabbageEra ->  Either FrameworkError   (Map TxIn ExecutionUnits)
evaluateExUnitMap  (DetailedChainInfo cpw conn pParam systemStart eraHistory ) usedUtxos txbody   = case eExUnits of
  Left tve -> Left $ FrameworkError   ExUnitCalculationError (show tve)
  Right map ->mapM  doMap ( Map.toList map) <&> Map.fromList

  where
    eExUnits=evaluateTransactionExecutionUnits BabbageEraInCardanoMode systemStart eraHistory pParam usedUtxos txbody
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


splitMetadataStrings :: Map Word64 A.Value -> Map Word64 A.Value
splitMetadataStrings = Map.map morphValue
  where
    morphValue :: A.Value -> A.Value
    morphValue  val = case val of
      A.Object (hm) ->  A.Object $ A.map  morphValue    hm
      A.Array vec ->A.Array (Vector.map  morphValue vec )
      A.String txt -> let txtList = stringToList Vector.empty txt in if length txtList <2 then A.String txt  else A.Array  txtList
      _ -> val

    -- Given a vecotr of Strings and Text, split the text into chunks of 64 bytes and append it into the vector as aeson String value. 
    stringToList :: Vector.Vector A.Value ->  T.Text -> Vector.Vector A.Value
    stringToList accum  txt = let
      splitted=splitString 0 T.empty txt
      (prefix,remaining) = splitted -- Debug.trace ("splitString " ++ show txt ++ " : " ++ show splitted ) splitted
      in if T.null txt then accum
          else stringToList (Vector.snoc accum (A.String prefix)) remaining

    -- given prefix string and it's length, take characters from txt until prefix has size almost <=64 chars
    splitString:: Int64 -> T.Text -> T.Text -> (T.Text,T.Text)
    splitString size prefix txt =  let
        tHead= T.head txt
        tHeadBS = LBS.length $  BSL.toLazyByteString $ toCharUtf8 tHead
        newSize= size + tHeadBS
        in  --Debug.trace ("Size of (" ++ (T.unpack prefix ) ++"," ++ if T.null txt then ""  else [tHead] ++  ") : " ++ show (size,newSize)) $ 
          if T.null txt  then (prefix,txt) else
              ( if  newSize > 64
                  then ( if  C.isSpace tHead  then (prefix,txt)
                          else case splitOnLastSpace prefix of
                                (txt', Nothing ) -> (prefix,txt)
                                (txtPre,Just txtEnd) -> (txtPre,  T.concat [txtEnd,txt] )
                        )
                  else   splitString newSize (  T.snoc  prefix tHead) (T.tail txt)
              )
    -- given text try to find the last space and split it . Also make sure that the split is not too big :D
    splitOnLastSpace :: T.Text -> (T.Text,Maybe T.Text)
    splitOnLastSpace txt = let
        end = T.takeWhileEnd  (not . C.isSpace) txt
        stripCount =  T.length end
        in if stripCount <=20  then (T.dropEnd stripCount  txt, Just end)
            else  (txt, Nothing)


toCharUtf8 :: Char -> BSL.Builder
toCharUtf8 = charUtf8