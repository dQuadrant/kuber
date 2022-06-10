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
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, TxOut, CurrencySymbol, MintingPolicy)
import Data.Aeson.Types (FromJSON(parseJSON), (.:), Parser)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.:?), (.!=), KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as A.Object
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import Debug.Trace (trace, traceM)
import qualified Data.HashMap.Strict as HM
import Data.String (IsString(fromString))
import qualified Debug.Trace as Debug
import qualified Data.Aeson as Aeson
import Data.Word (Word64)
import qualified Data.HashMap.Internal.Strict as H
import Data.Bifunctor


data TxMintingScript = TxSimpleScript ScriptInAnyLang
              | TxPlutusScript ScriptInAnyLang ScriptData (Maybe ExecutionUnits)
                            deriving(Show)

newtype TxValidatorScript = TxValidatorScript ScriptInAnyLang deriving (Show)

data TxInputResolved_ = TxInputUtxo (UTxO BabbageEra)
              | TxInputScriptUtxo TxValidatorScript ScriptData ScriptData (Maybe ExecutionUnits) (UTxO BabbageEra)
              | TxInputScriptUtxoInlineDatum TxValidatorScript  ScriptData (Maybe ExecutionUnits) (UTxO BabbageEra)
              deriving (Show)


data TxInputUnResolved_ = TxInputTxin TxIn
              | TxInputAddr (AddressInEra BabbageEra)
              | TxInputScriptTxin TxValidatorScript ScriptData ScriptData (Maybe ExecutionUnits) TxIn
              | TxInputScriptTxinInlineDatum TxValidatorScript  ScriptData (Maybe ExecutionUnits) TxIn
              deriving (Show)

data TxInput  = TxInputResolved TxInputResolved_ | TxInputUnResolved TxInputUnResolved_ deriving (Show)

data TxOutputContent =
     TxOutAddress (AddressInEra BabbageEra) Value
  |  TxOutScriptAddress (AddressInEra BabbageEra) Value (Hash ScriptData)
  |  TxOutScriptAddressWithData (AddressInEra BabbageEra) Value   ScriptData
  |  TxOutPkh PubKeyHash Value
  |  TxOutScript TxValidatorScript Value  (Hash ScriptData)
  |  TxOutScriptWithData TxValidatorScript Value   ScriptData  deriving (Show)


data TxOutput = TxOutput {
  content :: TxOutputContent,
  deductFee :: Bool,
  addChange :: Bool
} deriving (Show)

data TxCollateral =  TxCollateralTxin TxIn
                  |  TxCollateralUtxo (UTxO BabbageEra) deriving (Show)

data TxSignature =  TxSignatureAddr (AddressInEra BabbageEra)
                  | TxSignaturePkh PubKeyHash
                  | TxSignatureSkey (SigningKey PaymentKey)
                  deriving (Show)



data TxChangeAddr = TxChangeAddrUnset
                  | TxChangeAddr (AddressInEra BabbageEra) deriving (Show)

data TxInputSelection = TxSelectableAddresses [AddressInEra BabbageEra]
                  | TxSelectableUtxos  (UTxO BabbageEra)
                  | TxSelectableTxIn [TxIn]
                  | TxSelectableSkey [SigningKey PaymentKey]
                  deriving(Show)


data TxMintData = TxMintData PolicyId (ScriptWitness WitCtxMint BabbageEra) Value deriving (Show)

-- TxBuilder object
-- It is a semigroup and monoid instance, so it can be constructed using helper function
-- and merged to construct a transaction specification
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
    txDefaultChangeAddr :: Maybe (AddressInEra BabbageEra),
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
  ctxAvailableUtxo :: UTxO BabbageEra,
  ctxBuiler :: [TxBuilder]
}

txSelection :: TxInputSelection -> TxBuilder
txSelection v = TxBuilder  [v] [] [] [] Nothing Nothing [] [] Nothing Nothing Map.empty

txInput :: TxInput -> TxBuilder
txInput v = TxBuilder  [] [v] [] [] Nothing Nothing [] [] Nothing Nothing Map.empty

txMints :: [TxMintData] -> TxBuilder
txMints md= TxBuilder  [] [] [] [] Nothing Nothing md [] Nothing Nothing Map.empty

txOutput :: TxOutput -> TxBuilder
txOutput v =  TxBuilder  [] [] [v] [] Nothing Nothing [] [] Nothing Nothing Map.empty

txCollateral :: TxCollateral -> TxBuilder
txCollateral v =  TxBuilder  [] [] [] [v] Nothing Nothing [] [] Nothing Nothing Map.empty

txSignature :: TxSignature -> TxBuilder
txSignature v =  TxBuilder  [] [] [] [] Nothing Nothing [] [v] Nothing Nothing Map.empty



-- Transaction validity

-- Set validity Start and end time in posixMilliseconds
txValidPosixTimeRangeMs :: Integer -> Integer -> TxBuilder
txValidPosixTimeRangeMs start end = TxBuilder  [] [] [] [] (Just start) (Just end) [] [] Nothing Nothing Map.empty

-- set  validity statart time in posixMilliseconds
txValidFromPosixMs:: Integer -> TxBuilder
txValidFromPosixMs start =  TxBuilder  [] [] [] [] (Just start) Nothing [] [] Nothing Nothing Map.empty

-- set transaction validity end time in posixMilliseconds
txValidUntilPosixMs :: Integer -> TxBuilder
txValidUntilPosixMs end =  TxBuilder  [] [] [] [] Nothing (Just end) [] [] Nothing Nothing Map.empty

--- minting
txMint  v = txMints [v]

-- mint Simple Script
txMintSimpleScript :: SimpleScript SimpleScriptV2   ->   [(AssetName,Integer)] -> TxBuilder
txMintSimpleScript simpleScript amounts = txMint $ TxMintData policyId  witness (valueFromList  $ map (bimap (AssetId policyId) Quantity )  amounts )
  where
    witness=   SimpleScriptWitness SimpleScriptV2InBabbage SimpleScriptV2 simpleScript
    script = SimpleScript SimpleScriptV2 simpleScript
    policyId = scriptPolicyId script


-- pay to an Address
txPayTo:: AddressInEra BabbageEra ->Value ->TxBuilder
txPayTo addr v=  txOutput $  TxOutput (TxOutAddress  addr v) False False

-- pay to an Address by pubKeyHash. Note that the resulting address will be an enterprise address
txPayToPkh:: PubKeyHash  ->Value ->TxBuilder
txPayToPkh pkh v= txOutput $  TxOutput ( TxOutPkh  pkh  v ) False False

-- pay to Script address
txPayToScript :: AddressInEra BabbageEra -> Value -> Hash ScriptData -> TxBuilder
txPayToScript addr v d = txOutput $  TxOutput (TxOutScriptAddress  addr v d) False False

-- pay to script Address with datum added to the transaction
txPayToScriptWithData :: AddressInEra BabbageEra -> Value -> ScriptData -> TxBuilder
txPayToScriptWithData addr v d  = txOutput $ TxOutput  (TxOutScriptAddressWithData addr v  d) False False

-- input consmptions

-- use Utxo as input in the transaction
txConsumeUtxos :: UTxO BabbageEra -> TxBuilder
txConsumeUtxos utxo =  txInput $ TxInputResolved $  TxInputUtxo  utxo

-- use the TxIn as input in the transaction
-- the Txout value and address  is determined by querying the node
txConsumeTxIn :: TxIn -> TxBuilder
txConsumeTxIn  v = txInput $ TxInputUnResolved $ TxInputTxin v

-- use txIn as input in the transaction
-- Since TxOut is also given the txIn is not queried from the node.
txConsumeUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra -> TxBuilder
txConsumeUtxo tin v =txConsumeUtxos $ UTxO $ Map.singleton tin  v

-- Mark this address as txExtraKeyWitness in the transaction object.
txSignBy :: AddressInEra BabbageEra -> TxBuilder
txSignBy  a = txSignature (TxSignatureAddr a)

-- Mark this PublicKeyhash as txExtraKeyWitness in the transaction object.
txSignByPkh :: PubKeyHash  -> TxBuilder
txSignByPkh p = txSignature $ TxSignaturePkh p

-- Mark this signingKey's vKey as txExtraKey Witness in the transaction object.
-- When validating `txSignedBy` in plutus, this can be used to add the
txSign :: SigningKey PaymentKey -> TxBuilder
txSign p = txSignature $ TxSignatureSkey p
-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.


-- Redeem from a Script. The script address and value in the TxIn is determined automatically by querying the utxo from cardano node
txRedeemTxin:: TxIn -> ScriptInAnyLang ->ScriptData -> ScriptData  -> TxBuilder
txRedeemTxin txin script _data _redeemer = txInput $ TxInputUnResolved $ TxInputScriptTxin  (TxValidatorScript $ script)  _data  _redeemer  Nothing txin

-- Redeem from Script Address.
-- TxOut is provided so the address and value need not be queried from the caradno-node
txRedeemUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra -> ScriptInAnyLang  -> ScriptData  -> ScriptData -> TxBuilder
txRedeemUtxo txin txout script _data _redeemer = txInput $ TxInputResolved $ TxInputScriptUtxo  (TxValidatorScript $ script)  _data  _redeemer  Nothing $ UTxO $ Map.singleton txin  txout


 -- wallet addresses, from which utxos can be spent for balancing the transaction
txWalletAddresses :: [AddressInEra BabbageEra] -> TxBuilder
txWalletAddresses v = txSelection $ TxSelectableAddresses  v

-- wallet address, from which utxos can be spent  for balancing the transaction
txWalletAddress :: AddressInEra BabbageEra -> TxBuilder
txWalletAddress v = txWalletAddresses [v]

-- wallet utxos, that can be spent  for balancing the transaction
txWalletUtxos :: UTxO BabbageEra -> TxBuilder
txWalletUtxos v =  txSelection $  TxSelectableUtxos v

-- wallet utxo, that can be spent  for balancing the transaction
txWalletUtxo :: TxIn -> Cardano.Api.Shelley.TxOut CtxUTxO BabbageEra -> TxBuilder
txWalletUtxo tin tout = txWalletUtxos $  UTxO $ Map.singleton tin  tout

txWalletSignKey :: SigningKey PaymentKey -> TxBuilder
txWalletSignKey s= txWalletSignKeys [s]

txWalletSignKeys :: [SigningKey PaymentKey] -> TxBuilder
txWalletSignKeys s= txSelection $ TxSelectableSkey s
