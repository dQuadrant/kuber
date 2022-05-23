{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Kuber.Server.Core where

import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy             as TL
import Cardano.Api
import Control.Exception (throw, try)
import qualified Data.Set as Set
import System.Exit (die)
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import System.Environment (getEnv)
import System.FilePath (joinPath)
import Cardano.Ledger.Alonzo.Scripts (ExUnits(ExUnits))
import Data.Text.Conversions (Base16(Base16), convertText)
import Cardano.Api.Shelley (TxBody(ShelleyTxBody), fromShelleyTxIn)
import Cardano.Ledger.Shelley.API (TxBody(_inputs))
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Alonzo.TxBody (inputs')
import qualified Data.Map as Map
import Data.Text (Text)
import Cardano.Kuber.Data.Models



getBalance :: ChainConnectInfo -> String -> IO BalanceResponse
getBalance ctx addrStr = do
  addr <- case deserialiseAddress AsAddressAny $ T.pack addrStr of
    Nothing -> throw $ FrameworkError  ParserError  "Invalid address"
    Just aany -> pure aany
  eUtxos <- queryUtxos (getConnectInfo ctx) $ Set.singleton addr
  case eUtxos of 
    Left fe -> throw fe
    Right utxos -> pure $ BalanceResponse  utxos

submitTx :: ChainConnectInfo -> SubmitTxModal -> IO TxResponse
submitTx ctx (SubmitTxModal tx mWitness) = do
  let tx' = case mWitness of
        Nothing -> tx
        Just kw -> makeSignedTransaction (kw : getTxWitnesses tx) txbody
      txbody = getTxBody tx
  executeSubmitTx (getConnectInfo ctx) tx'
  pure $ TxResponse tx'

txBuilder :: DetailedChainInfo  ->  TxBuilder -> IO TxResponse
txBuilder dcinfo txBuilder = do
  let encodedTxBuilder = A.encode txBuilder
  let txBuilderStr = TL.unpack $ TL.decodeUtf8 encodedTxBuilder
  print txBuilderStr
  txBodyE<-txBuilderToTxBody dcinfo txBuilder
  case txBodyE of 
    Left fe -> throw fe
    Right txBody -> pure $ TxResponse $ makeSignedTransaction [] txBody

testTx :: DetailedChainInfo  ->  TxModal -> IO TxResponse
testTx dcinfo txBuilder = do
    fail "sad"
  -- let encodedTxBuilder = A.encode txBuilder
  -- let txBuilderStr = TL.unpack $ TL.decodeUtf8 encodedTxBuilder
  -- print txBuilderStr
  -- txBodyE<-txBuilderToTxBody dcinfo txBuilder
  -- case txBodyE of 
  --   Left fe -> throw fe
  --   Right txBody -> do
  --     signKey <- getDefaultSignKey
  --     print txBody
  --     let keyWit = makeShelleyKeyWitness txBody (WitnessPaymentKey signKey)
  --         tx = makeSignedTransaction [keyWit] txBody
  --     executeSubmitTx (getConnectInfo dcinfo) tx
  --     pure $ TxResponse tx


evaluateExecutionUnits' :: DetailedChainInfo ->  String -> IO [Either String ExecutionUnits]
evaluateExecutionUnits' dcinfo  txStr = do 
      case convertText txStr of
        Nothing -> fail "Tx string is not hex encoded"
        Just (Base16 bs) -> case deserialiseFromCBOR (AsTx AsAlonzoEra ) bs of
          Left  e -> fail $ "Tx string: Invalid CBOR format : "++ show e
          Right tx -> evaluateExecutionUnits dcinfo  tx
