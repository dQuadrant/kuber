{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Websocket.Commands where

import Cardano.Api
import Cardano.Api.Experimental hiding (ConwayEra)
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models (UtxoModal)
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
-- import Websocket.Aeson (ProtocolParams)

import Cardano.Ledger.Core
import Control.Exception
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import Data.ByteString.Char8 as BS8 hiding (head, map)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor
import Data.Monoid (Any)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Conversions
import Data.Text.Encoding
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Debug.Trace as Debug
import GHC.Generics
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple
import qualified Network.WebSockets as WS
import System.Environment
import Websocket.Forwarder
import Websocket.SocketConnection
import Websocket.Utils

-- import qualified Control.Lens.Internal.Deque as Set
data HydraCommitTx = HydraCommitTx
  { cborHex :: String,
    description :: String,
    txId :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

hydraHeadMessageForwardingFailed :: T.Text
hydraHeadMessageForwardingFailed = T.pack "Failed to forward message to hydra head"

initialize :: IO T.Text
initialize = do
  sendCommandToHydraNodeSocket InitializeHead

abort :: IO T.Text
abort = do
  sendCommandToHydraNodeSocket Abort

queryUTxO :: IO T.Text
queryUTxO = do
  sendCommandToHydraNodeSocket GetUTxO

-- commitUTxO :: [T.Text] -> A.Value -> IO T.Text
commitUTxO :: [T.Text] -> Aeson.Value -> IO T.Text
commitUTxO utxos sk = do
  utxoSchema <- createUTxOSchema utxos
  signKey <- case parseSignKey (jsonToText sk) of
    Just parsedSk -> pure parsedSk
    Nothing -> error "Failure parsing singing key"
  unsignedCommitTx <- getHydraCommitTx utxoSchema
  let txCbor = T.pack $ case A.eitherDecode (fromStrict $ encodeUtf8 unsignedCommitTx) of
        Right (HydraCommitTx cborHex _ _) -> cborHex
        Left _ -> error "Error decoding hydra response"
  let hydraWitness = case convertText txCbor <&> unBase16 of
        Nothing -> error ""
        Just bs -> case deserialiseFromCBOR (AsTx AsConwayEra) bs of
          Left dc -> Debug.trace (show dc) $ error "commitUTxO: Failure parsing Tx"
          Right tx -> snd $ getTxBodyAndWitnesses tx
  signedTx <- case convertText txCbor <&> unBase16 of
    Nothing -> error ""
    Just bs ->
      case deserialiseFromCBOR (AsTx bAsEra) bs of
        Left dc -> Debug.trace (show dc) $ error "commitUTxO: Failure parsing Tx"
        Right tx -> pure $ makeSignedTransaction (hydraWitness ++ [(makeShelleyKeyWitness shelleyBasedEra (getTxBody tx) . WitnessPaymentKey) signKey]) (getTxBody tx)
  let submittedTx = kSubmitTx (InAnyCardanoEra ConwayEra signedTx)
  submitHandler submittedTx

getHydraCommitTx :: T.Text -> IO T.Text
getHydraCommitTx utxoSchema = do
  let jsonResponse = textToJSON utxoSchema
  post "commit" jsonResponse

createUTxOSchema :: [T.Text] -> IO T.Text
createUTxOSchema utxos = do
  localChain <- chainInfoFromEnv
  result <- evaluateKontract localChain (getUtxoDetails @ChainConnectInfo @ConwayEra utxos)
  case result of
    Left err -> error $ "Query failed: " <> show err
    Right res -> pure $ (T.pack . BS8.unpack . BSL.toStrict . A.encode) res

getUtxoDetails ::
  (HasChainQueryAPI a, IsTxBuilderEra era) =>
  [T.Text] ->
  Kontract a w FrameworkError (UTxO era)
getUtxoDetails utxoList = do
  parsed <-
    mapM
      ( \x -> case parseTxIn x of
          Just txin -> pure txin
          Nothing -> error ""
      )
      utxoList
  kQueryUtxoByTxin (Set.fromList parsed)

-- getProtocolParameters :: IO (PParams (ShelleyLedgerEra ConwayEra))
getProtocolParameters :: IO A.Value
getProtocolParameters = do
  hydraProtocolParameters <- fetch >>= \query -> query (T.pack "protocol-parameters")
  pure $ textToJSON hydraProtocolParameters

-- Debug.traceM(T.unpack hydraProtocolParameters)
-- pure $ parseProtocolParameters hydraProtocolParameters

submitHandler tx = do
  localChain <- chainInfoFromEnv
  result <- evaluateKontract localChain tx
  case result of
    Left fe -> pure $ T.pack $ BS8.unpack $ prettyPrintJSON fe
    Right tx -> pure $ T.pack $ BS8.unpack $ prettyPrintJSON tx