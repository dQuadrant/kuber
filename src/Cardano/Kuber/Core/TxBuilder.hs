{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.Kuber.Core.TxBuilder

where


import Cardano.Api hiding(txMetadata, txFee)
import Cardano.Api.Shelley hiding (txMetadata, txFee)
import Cardano.Kuber.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Kuber.Util
import Data.Functor ((<&>))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)

import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, TxOut, CurrencySymbol)
import Data.Aeson.Types (FromJSON(parseJSON), (.:), Parser)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.:?), (.!=), KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as A.Object
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy             as TL
import Debug.Trace (trace, traceM)
import qualified Data.HashMap.Strict as HM
import Data.String (IsString(fromString))
import qualified Debug.Trace as Debug
import qualified Data.Aeson as Aeson
import Data.Word (Word64)
import qualified Data.HashMap.Internal.Strict as H

-- mktx 
data TxMintingScript = TxSimpleScript ScriptInAnyLang
              | TxPlutusScript ScriptInAnyLang ScriptData (Maybe ExecutionUnits)
                            deriving(Show)

newtype TxValidatorScript = TxValidatorScript ScriptInAnyLang deriving (Show)

data TxInputResolved_ = TxInputUtxo (UTxO AlonzoEra)
              | TxInputScriptUtxo TxValidatorScript ScriptData ScriptData (Maybe ExecutionUnits) (UTxO AlonzoEra) deriving (Show)
data TxInputUnResolved_ = TxInputTxin TxIn
              | TxInputAddr (AddressInEra AlonzoEra)
              | TxInputScriptTxin TxValidatorScript ScriptData ScriptData (Maybe ExecutionUnits) TxIn deriving (Show)

data TxInput  = TxInputResolved TxInputResolved_ | TxInputUnResolved TxInputUnResolved_ deriving (Show)

data TxOutputContent =
     TxOutAddress (AddressInEra AlonzoEra) Value
  |  TxOutScriptAddress (AddressInEra AlonzoEra) Value (Hash ScriptData)
  |  TxOutPkh PubKeyHash Value
  |  TxOutScript TxValidatorScript Value  (Hash ScriptData)  deriving (Show)

data TxOutput = TxOutput {
  content :: TxOutputContent,
  addChange :: Bool,
  deductFee :: Bool
} deriving (Show)

data TxCollateral =  TxCollateralTxin TxIn
                  |  TxCollateralUtxo (UTxO AlonzoEra)
    deriving (Show)

data TxSignature =  TxSignatureAddr (AddressInEra AlonzoEra)
                  | TxSignaturePkh PubKeyHash
    deriving (Show)


data TxChangeAddr = TxChangeAddrUnset
                  | TxChangeAddr (AddressInEra AlonzoEra)
   deriving (Show)

data TxInputSelection = TxSelectableAddresses [AddressInEra AlonzoEra]
                  | TxSelectableUtxos  (UTxO AlonzoEra)
                  | TxSelectableTxIn [TxIn]
                   deriving(Show)

data TxMintData = TxMintData PolicyId (ScriptWitness WitCtxMint AlonzoEra) Value deriving (Show)

data TxBuilder=TxBuilder{
    txSelections :: [TxInputSelection],
    txInputs:: [TxInput],
    txOutputs :: [TxOutput],
    txCollaterals :: [TxCollateral],  -- collateral for the transaction
    txValidityStart :: Maybe Integer,
    txValidityEnd :: Maybe Integer,
    txMintData :: [TxMintData],
    txSignatures :: [TxSignature],
    txFee :: Maybe Integer,
    txDefaultChangeAddr :: Maybe (AddressInEra AlonzoEra),
    txMetadata :: Map Word64 Aeson.Value
  } deriving (Show)

instance Monoid TxBuilder where
  mempty = TxBuilder  [] [] [] [] Nothing Nothing [] [] Nothing Nothing Map.empty

instance Semigroup TxBuilder where
  (<>)  txb1 txb2 =TxBuilder{
    txSelections = txSelections txb1 ++ txSelections txb2,
    txInputs = txInputs txb1 ++ txInputs txb2,
    txOutputs = txOutputs txb1 ++ txOutputs txb2,
    txCollaterals  = txCollaterals txb1 ++ txCollaterals txb2,  -- collateral for the transaction
    txValidityStart = case txValidityStart txb1 of
          Just v1 -> case txValidityStart txb2 of
            Just v2 -> Just $ min v1 v2
            Nothing -> Just v1
          Nothing -> txValidityStart txb2,
    txValidityEnd = case txValidityEnd txb1 of
      Just v1 -> case txValidityEnd txb2 of
        Just v2 -> Just $ max v1 v2
        _ -> Just v1
      _ -> txValidityEnd txb2,
    txMintData = txMintData txb2 <> txMintData txb2,
    txSignatures = txSignatures txb1 ++ txSignatures txb2,
    txFee  = case txFee txb1 of
      Just f -> case txFee txb2 of
        Just f2 -> Just $ max f f2
        _ -> Just f
      Nothing -> txFee txb2,
    txDefaultChangeAddr = case txDefaultChangeAddr txb1 of
      Just addr -> Just addr
      _ -> txDefaultChangeAddr txb2,
    txMetadata = txMetadata txb1 <> txMetadata txb2
  }


data TxContext = TxContext {
  ctxAvailableUtxo :: UTxO AlonzoEra,
  ctxBuiler :: [TxBuilder]
}

txSelection :: TxInputSelection -> TxBuilder
txSelection v = TxBuilder  [v] [] [] [] Nothing Nothing [] [] Nothing Nothing Map.empty

txInput :: TxInput -> TxBuilder
txInput v = TxBuilder  [] [v] [] [] Nothing Nothing [] [] Nothing Nothing Map.empty

txOutput :: TxOutput -> TxBuilder
txOutput v =  TxBuilder  [] [] [v] [] Nothing Nothing [] [] Nothing Nothing Map.empty

txCollateral :: TxCollateral -> TxBuilder
txCollateral v =  TxBuilder  [] [] [] [v] Nothing Nothing [] [] Nothing Nothing Map.empty

txSignature :: TxSignature -> TxBuilder
txSignature v =  TxBuilder  [] [] [] [] Nothing Nothing [] [v] Nothing Nothing Map.empty

txValidPosixTimeRangeMs :: Integer -> Integer -> TxBuilder
txValidPosixTimeRangeMs start end = TxBuilder  [] [] [] [] (Just start) (Just end) [] [] Nothing Nothing Map.empty

txValidFromPosixMs:: Integer -> TxBuilder
txValidFromPosixMs start =  TxBuilder  [] [] [] [] (Just start) Nothing [] [] Nothing Nothing Map.empty

txValidUntilPosixMs :: Integer -> TxBuilder
txValidUntilPosixMs end =  TxBuilder  [] [] [] [] Nothing (Just end) [] [] Nothing Nothing Map.empty

txMint :: [TxMintData] -> TxBuilder
txMint md= TxBuilder  [] [] [] [] Nothing Nothing md [] Nothing Nothing Map.empty

-- payment contexts

txPayTo:: AddressInEra AlonzoEra ->Value ->TxBuilder
txPayTo addr v=  txOutput $  TxOutput (TxOutAddress  addr v) False False

txPayToPkh:: PubKeyHash  ->Value ->TxBuilder
txPayToPkh pkh v= txOutput $  TxOutput ( TxOutPkh  pkh  v ) False False

txPayToScript :: AddressInEra AlonzoEra -> Value -> Hash ScriptData -> TxBuilder
txPayToScript addr v d = txOutput $  TxOutput (TxOutScriptAddress  addr v d) False False

txPayToScriptWithData :: AddressInEra AlonzoEra -> Value -> ScriptData -> TxBuilder
txPayToScriptWithData addr v d  = txOutput $ TxOutput  (TxOutScriptAddress addr v (hashScriptData d)) False False

-- input consmptions

txConsumeUtxos :: UTxO AlonzoEra -> TxBuilder
txConsumeUtxos utxo =  txInput $ TxInputResolved $  TxInputUtxo  utxo

txConsumeTxIn :: TxIn -> TxBuilder
txConsumeTxIn  v = txInput $ TxInputUnResolved $ TxInputTxin v

txConsumeUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO AlonzoEra -> TxBuilder
txConsumeUtxo tin v =txConsumeUtxos $ UTxO $ Map.singleton tin  v

txSignBy :: AddressInEra AlonzoEra -> TxBuilder
txSignBy  a = txSignature (TxSignatureAddr a)

txSignByPkh :: PubKeyHash  -> TxBuilder
txSignByPkh p = txSignature $ TxSignaturePkh p
-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.


-- Redeem from Script Address.
txRedeemTxin:: TxIn -> ScriptInAnyLang ->ScriptData -> ScriptData  -> TxBuilder
txRedeemTxin txin script _data _redeemer = txInput $ TxInputUnResolved $ TxInputScriptTxin  (TxValidatorScript $ script)  _data  _redeemer  Nothing txin

-- Redeem from Script Address.
txRedeemUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO AlonzoEra -> ScriptInAnyLang  -> ScriptData  -> ScriptData -> TxBuilder
txRedeemUtxo txin txout script _data _redeemer = txInput $ TxInputResolved $ TxInputScriptUtxo  (TxValidatorScript $ script)  _data  _redeemer  Nothing $ UTxO $ Map.singleton txin  txout


 -- wallet configuration
txWalletAddresses :: [AddressInEra AlonzoEra] -> TxBuilder
txWalletAddresses v = txSelection $ TxSelectableAddresses  v

txWalletAddress :: AddressInEra AlonzoEra -> TxBuilder
txWalletAddress v = txWalletAddresses [v]

txWalletUtxos :: UTxO AlonzoEra -> TxBuilder
txWalletUtxos v =  txSelection $  TxSelectableUtxos v

txWalletUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO AlonzoEra -> TxBuilder
txWalletUtxo tin tout = txWalletUtxos $  UTxO $ Map.singleton tin  tout
