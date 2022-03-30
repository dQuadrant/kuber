{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Cardano.Contrib.Kubær.TxBuilder
where


import Cardano.Api hiding(txFee)
import Cardano.Api.Shelley hiding (txFee)
import Cardano.Contrib.Kubær.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Contrib.Kubær.Util
import Data.Functor ((<&>))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)

import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, TxOut)

-- mktx 
newtype TxMintingScript = TxMintintScript ScriptInAnyLang deriving(Show)
newtype TxValidatorScript = TxValidatorScript ScriptInAnyLang deriving (Show)

data TxInputResolved_ = TxInputUtxo (UTxO AlonzoEra)
              | TxInputScriptUtxo TxValidatorScript ScriptData ScriptData (Maybe ExecutionUnits) (UTxO AlonzoEra) deriving (Show)
data TxInputUnResolved_ = TxInputTxin TxIn
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

data TxSignature =  TxExtraSignature Integer   
                  | TxSignatureAddr (AddressInEra AlonzoEra)
                  | TxSignaturePkh PubKeyHash 
    deriving (Show)


data TxChangeAddr = TxChangeAddrUnset 
                  | TxChangeAddr (AddressInEra AlonzoEra)
   deriving (Show)

data TxInputSelection = TxSelectableAddresses [AddressInEra AlonzoEra]
                  | TxSelectableUtxos  (UTxO AlonzoEra) deriving(Show)

data TxBuilder=TxBuilder{
    txSelections :: [TxInputSelection],
    txInputs:: [TxInput],
    txOutputs :: [TxOutput],
    txMintingScripts :: [TxMintingScript],
    txCollaterals :: [TxCollateral],  -- collateral for the transaction
    txValidityStart :: Maybe Integer,
    txValidityEnd :: Maybe Integer,
    _txMint :: Value,
    txSignatures :: [TxSignature],
    txFee :: Maybe Integer,
    txDefaultChangeAddr :: Maybe (AddressInEra AlonzoEra)
  } deriving (Show)

instance Monoid TxBuilder where
  mempty = TxBuilder  [] [] [] [] [] Nothing Nothing (valueFromList []) [] Nothing Nothing

instance Semigroup TxBuilder where
  (<>)  txb1 txb2 =TxBuilder{
    txSelections = txSelections txb1 ++ txSelections txb2,
    txInputs = txInputs txb1 ++ txInputs txb2,
    txOutputs = txOutputs txb1 ++ txOutputs txb2,
    txMintingScripts = txMintingScripts txb1 ++ txMintingScripts txb2 ,
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
    _txMint = _txMint txb2 <> _txMint txb2,
    txSignatures = txSignatures txb1 ++ txSignatures txb2,
    txFee  = case txFee txb1 of 
      Just f -> case txFee txb2 of 
        Just f2 -> Just $ max f f2
        _ -> Just f
      Nothing -> txFee txb2,
    txDefaultChangeAddr = case txDefaultChangeAddr txb1 of 
      Just addr -> Just addr 
      _ -> txDefaultChangeAddr txb2
  }


data TxContext = TxContext {
  ctxAvailableUtxo :: UTxO AlonzoEra,
  ctxBuiler :: [TxBuilder]
}

ctxSelection v = TxBuilder  [v] [] [] [] [] Nothing Nothing (valueFromList []) [] Nothing Nothing
 
ctxInput v = TxBuilder  [] [v] [] [] [] Nothing Nothing (valueFromList []) [] Nothing Nothing

ctxOutput v =  TxBuilder  [] [] [v] [] [] Nothing Nothing (valueFromList []) [] Nothing Nothing

txCollateral v =  TxBuilder  [] [] [] [] [v] Nothing Nothing (valueFromList []) [] Nothing Nothing

txValidPosixTimeRange start end = TxBuilder  [] [] [] [] [] (Just start) (Just end) (valueFromList []) [] Nothing Nothing

txValidFromPosixTime start =  TxBuilder  [] [] [] [] [] (Just start) Nothing (valueFromList []) [] Nothing Nothing

txValidUntilPosixTime end =  TxBuilder  [] [] [] [] [] Nothing (Just end) (valueFromList []) [] Nothing Nothing

txMint v= TxBuilder  [] [] [] [] [] Nothing Nothing v [] Nothing Nothing

-- payment contexts

txPayTo:: AddressInEra AlonzoEra ->Value ->TxBuilder
txPayTo addr v=  ctxOutput $  TxOutput (TxOutAddress  addr v) False False

txPayToPkh:: PubKeyHash  ->Value ->TxBuilder
txPayToPkh pkh v= ctxOutput $  TxOutput ( TxOutPkh  pkh  v ) False False

txPayToScript addr v d = ctxOutput $  TxOutput (TxOutScriptAddress  addr v d) False False

-- input consmptions

txConsumeUtxos :: UTxO AlonzoEra -> TxBuilder
txConsumeUtxos utxo =  ctxInput $ TxInputResolved $  TxInputUtxo  utxo

txConsumeTxIn :: TxIn -> TxBuilder
txConsumeTxIn  v = ctxInput $ TxInputUnResolved $ TxInputTxin v

txConsumeUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO AlonzoEra -> TxBuilder
txConsumeUtxo tin v =txConsumeUtxos $ UTxO $ Map.singleton tin  v

-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.


-- Redeem from Script Address.
txRedeemTxin:: TxIn -> ScriptInAnyLang ->ScriptData -> ScriptData  -> TxBuilder
txRedeemTxin txin script _data _redeemer = ctxInput $ TxInputUnResolved $ TxInputScriptTxin  (TxValidatorScript $ script)  _data  _redeemer  Nothing txin 

-- Redeem from Script Address.
txRedeemUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO AlonzoEra -> ScriptInAnyLang  -> ScriptData  -> ScriptData -> TxBuilder
txRedeemUtxo txin txout script _data _redeemer = ctxInput $ TxInputResolved $ TxInputScriptUtxo  (TxValidatorScript $ script)  _data  _redeemer  Nothing $ UTxO $ Map.singleton txin  txout

txPayerAddresses :: [AddressInEra AlonzoEra] -> TxBuilder
txPayerAddresses v = ctxSelection $ TxSelectableAddresses  v

txPayerAddress v = txPayerAddresses [v]

txAvailableUtxos :: UTxO AlonzoEra -> TxBuilder
txAvailableUtxos v =  ctxSelection $  TxSelectableUtxos v

txAvailableUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO AlonzoEra -> TxBuilder
txAvailableUtxo tin tout = txAvailableUtxos $  UTxO $ Map.singleton tin  tout
