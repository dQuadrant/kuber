{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.ByteString.Lazy.Char8 as BL
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

hydraHeadMessageForwardingFailed :: T.Text
hydraHeadMessageForwardingFailed = T.pack "Failed to forward message to hydra head"

-- Function to initialize Hydra by sending { "tag": "Init" } and waiting for a response

initialize :: IO T.Text
initialize = do 
  sendCommandToHydraNodeSocket InitializeHead

abort :: IO T.Text
abort = do
  sendCommandToHydraNodeSocket Abort

queryUTxO :: IO T.Text
queryUTxO = do
  sendCommandToHydraNodeSocket GetUTxO

-- hydraResponse <- sendCommandToHydraNodeSocket GetUTxO
-- case hydraResponse of
--   Nothing -> pure $ hydraHeadMessageForwardingFailed
--   Just a -> pure a

-- close :: IO ()
-- close = sendMessage >>= \send -> send (T.pack "{\"tag\": \"Close\"}")

-- getUTxO :: IO ()
-- getUTxO = sendMessage >>= \send -> send (T.pack "{\"tag\": \"GetUTxO\"}")

-- getProtocolParameters :: IO (PParams (ShelleyLedgerEra ConwayEra))
getProtocolParameters :: IO A.Value
getProtocolParameters = do
  hydraProtocolParameters <- fetch >>= \query -> query (T.pack "protocol-parameters")
  pure $ textToJSON hydraProtocolParameters

-- Debug.traceM(T.unpack hydraProtocolParameters)
-- pure $ parseProtocolParameters hydraProtocolParameters
