{-# LANGUAGE ScopedTypeVariables #-}
module Core where

import qualified Data.Text as T
import Cardano.Contrib.Kubær.TxBuilder (TxBuilder)
import qualified Data.Aeson as A
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy             as TL
import Cardano.Contrib.Kubær.Models (BalanceResponse (BalanceResponse), TxResponse (TxResponse), SubmitTxModal (SubmitTxModal))
import Cardano.Contrib.Kubær.ChainInfo (ChainConnectInfo, ChainInfo (getConnectInfo), DetailedChainInfo)
import Cardano.Contrib.Kubær.Util (queryUtxos, executeSubmitTx, readSignKey)
import Cardano.Api
import Control.Exception (throw, try)
import Cardano.Contrib.Kubær.Error (FrameworkError(FrameworkError), ErrorType (ParserError, LibraryError))
import qualified Data.Set as Set
import System.Exit (die)
import Cardano.Contrib.Kubær.TxFramework (txBuilderToTxBody)
import System.Environment (getEnv)
import System.FilePath (joinPath)

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

testTx :: DetailedChainInfo  ->  TxBuilder -> IO TxResponse
testTx dcinfo txBuilder = do
  let encodedTxBuilder = A.encode txBuilder
  let txBuilderStr = TL.unpack $ TL.decodeUtf8 encodedTxBuilder
  print txBuilderStr
  txBodyE<-txBuilderToTxBody dcinfo txBuilder
  case txBodyE of 
    Left fe -> throw fe
    Right txBody -> do
      signKey <- getDefaultSignKey
      print txBody
      let keyWit = makeShelleyKeyWitness txBody (WitnessPaymentKey signKey)
          tx = makeSignedTransaction [keyWit] txBody
      executeSubmitTx (getConnectInfo dcinfo) tx
      pure $ TxResponse tx

getDefaultSignKey :: IO (SigningKey PaymentKey)
getDefaultSignKey= getWorkPath ["default.skey"] >>= readSignKey

getWorkPath :: [FilePath] -> IO  FilePath
getWorkPath paths= do
  eitherHome <-try $ getEnv "HOME"
  case eitherHome of
    Left (e::IOError) -> throw $ FrameworkError LibraryError "Can't get Home directory"
    Right home -> do
      pure $ joinPath  $  [home , ".cardano"] ++ paths