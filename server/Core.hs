module Core where

import qualified Data.Text as T
import Cardano.Contrib.Kubær.TxBuilder (TxBuilder)
import qualified Data.Aeson as A
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy             as TL
import Cardano.Contrib.Kubær.Models (BalanceResponse (BalanceResponse), TxResponse (TxResponse), SubmitTxModal (SubmitTxModal))
import Cardano.Contrib.Kubær.ChainInfo (ChainConnectInfo, ChainInfo (getConnectInfo), DetailedChainInfo)
import Cardano.Contrib.Kubær.Util (queryUtxos, executeSubmitTx)
import Cardano.Api
import Control.Exception (throw)
import Cardano.Contrib.Kubær.Error (FrameworkError(FrameworkError), ErrorType (ParserError))
import qualified Data.Set as Set
import System.Exit (die)
import Cardano.Contrib.Kubær.TxFramework (txBuilderToTxBody)

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
  v<-txBuilderToTxBody dcinfo txBuilder
  case v of 
    Left fe -> throw fe
    Right tb -> pure $ TxResponse $ makeSignedTransaction [] tb
