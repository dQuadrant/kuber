{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api.Spec where

import Cardano.Api
import Cardano.Api.Shelley (ShelleyLedgerEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.String
import Data.Text hiding (map)
import qualified Data.Text as T hiding (map)
import GHC.Generics
import GHC.IO (unsafePerformIO)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Websocket.Aeson
import Websocket.Commands
import Websocket.Utils
import qualified Debug.Trace as Debug


-- Define CORS policy
corsMiddlewarePolicy :: CorsResourcePolicy
corsMiddlewarePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods = [BS.pack "GET", BS.pack "POST", BS.pack "OPTIONS"],
      corsRequestHeaders = [fromString "content-type", fromString "api-key"],
      corsExposedHeaders = Nothing,
      corsMaxAge = Just 3600,
      corsVaryOrigin = True,
      corsRequireOrigin = False,
      corsIgnoreFailures = True
    }

newtype ResponseMessage = ResponseMessage
  { result :: String
  }
  deriving (Show, Generic)

instance ToJSON ResponseMessage

newtype CommitUTxOs = CommitUTxOs
  { commit :: [String]
  }
  deriving (Show, Generic, FromJSON, ToJSON)


type API =
  "hydra" :> (HydraCommands)

type HydraCommands =
  "init" :> Get '[JSON] A.Value
    :<|> "abort" :> Get '[JSON] A.Value
    :<|> "query" :> "utxo" :> Get '[JSON] A.Value
    :<|> "commit" :> ReqBody '[JSON] CommitUTxOs :> Post '[JSON] [A.Value]
    :<|> "protocol-parameters" :> Get '[JSON] A.Value

-- Define Handlers
server :: Server API
server = initHandler :<|> abortHandler :<|> queryUtxoHandler :<|> commitHandler :<|> protocolParameterHandler

-- initHandler :: Handler Text
initHandler :: Handler A.Value
initHandler = do
  initResponse <- liftIO $ initialize
  let jsonResponse = textToJSON initResponse
  return jsonResponse

abortHandler :: Handler A.Value
abortHandler = do
  abortResponse <- liftIO $ abort
  let jsonResponse = textToJSON abortResponse
  return jsonResponse

queryUtxoHandler :: Handler A.Value
queryUtxoHandler = do
  queryUtxoResponse <- liftIO $ queryUTxO
  let jsonResponse = textToJSON queryUtxoResponse
  return jsonResponse

commitHandler :: CommitUTxOs -> Handler [A.Value]
commitHandler commits = do 
  commitResponse <- liftIO $ commitUTxO (map T.pack $ commit commits)
  let jsonResponse = map textToJSON commitResponse
  return jsonResponse

protocolParameterHandler :: Handler (A.Value)
protocolParameterHandler = do
  params <- liftIO $ getProtocolParameters
  return params

-- Create API Proxy
deployAPI :: Proxy API
deployAPI = Proxy

-- Define Hydra application
hydraApp :: Application
hydraApp = cors (const $ Just corsMiddlewarePolicy) $ serve deployAPI server

-- Run the server
main :: IO ()
main = run 8080 hydraApp
