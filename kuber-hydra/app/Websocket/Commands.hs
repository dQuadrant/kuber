{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Websocket.Commands where

import Cardano.Api
import Cardano.Api.Experimental
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models (UtxoModal)
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
import Cardano.Ledger.Core
-- import Websocket.Aeson (ProtocolParams)

import Control.Exception
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import Data.ByteString.Char8 as BS8 hiding (map)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Monoid (Any)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Debug.Trace as Debug
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple
import qualified Network.WebSockets as WS
import System.Environment
import Websocket.Forwarder
import Websocket.SocketConnection
import Websocket.Utils

-- import qualified Control.Lens.Internal.Deque as Set

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

commitUTxO :: [T.Text] -> IO T.Text
commitUTxO utxos = do
  utxoSchema <- createUTxOSchema utxos
  getHydraCommitTx utxoSchema

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
