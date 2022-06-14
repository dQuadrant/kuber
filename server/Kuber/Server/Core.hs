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
import qualified Data.ByteString.Char8 as BS8
import Data.Functor ((<&>))
import Servant (Handler)
import Control.Monad.IO.Class (liftIO)



getBalance :: ChainInfo x => x -> String -> IO BalanceResponse
getBalance ctx addrStr = do
  addr <- case deserialiseAddress AsAddressAny $ T.pack addrStr of
    Nothing -> case
        deserialiseFromBech32 (AsSigningKey AsPaymentKey) $ T.pack addrStr of
      Left bde ->       throw $ FrameworkError  ParserError  "Invalid address"
      Right any -> pure $ toAddressAny $ skeyToAddr   any (getNetworkId ctx)
    Just aany -> pure aany
  eUtxos <- queryUtxos (getConnectInfo ctx) $ Set.singleton addr
  case eUtxos of
    Left fe -> throw fe
    Right utxos -> pure $ BalanceResponse  utxos

submitTx :: ChainInfo x =>  x -> SubmitTxModal -> IO TxResponse
submitTx ctx (SubmitTxModal tx mWitness) = do
  let tx' = case mWitness of
        Nothing -> tx
        Just kw -> makeSignedTransaction (kw : getTxWitnesses tx) txbody
      txbody = getTxBody tx
  status <- executeSubmitTx (getConnectInfo ctx) tx'
  case status of
    Left fe -> throw fe
    Right x1 ->   pure $ TxResponse tx'

txBuilder :: DetailedChainInfo  ->  Maybe Bool -> TxBuilder -> IO TxResponse
txBuilder dcinfo submitM txBuilder = do
  putStrLn $ BS8.unpack $  prettyPrintJSON $ txBuilder
  txE <- txBuilderToTxIO dcinfo txBuilder
  case txE of
    Left fe -> throw fe
    Right tx -> case submitM of
      Nothing ->   pure $ TxResponse tx
      Just submit -> do
        txE <- executeSubmitTx (getConnectInfo dcinfo) tx
        case txE of
          Left fe -> throw fe
          Right _ ->   pure $ TxResponse tx


evaluateExecutionUnits' :: DetailedChainInfo ->  String -> IO [Either String ExecutionUnits]
evaluateExecutionUnits' dcinfo  txStr = do
      case convertText txStr of
        Nothing -> fail "Tx string is not hex encoded"
        Just (Base16 bs) -> case deserialiseFromCBOR (AsTx AsAlonzoEra ) bs of
          Left  e -> fail $ "Tx string: Invalid CBOR format : "++ show e
          Right tx -> evaluateExecutionUnits dcinfo  tx
