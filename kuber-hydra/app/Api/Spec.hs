{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Api.Spec where

import Cardano.Api
import Cardano.Kuber.Api
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String
import qualified Data.Text as T hiding (map)
import GHC.Generics
import Network.HTTP.Types (status400)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Rewrite
import Network.Wai.Middleware.Static
import Servant
import Servant.Exception
import Websocket.Commands
import Websocket.Utils

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

data CommitUTxOs = CommitUTxOs
  { utxos :: [String],
    signKey :: A.Value
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance ToServantErr FrameworkError where
  status (FrameworkError _ _) = status400
  status (FrameworkErrors _) = status400

instance MimeRender PlainText FrameworkError where
  mimeRender ct = mimeRender ct . show

type API = HydraAPI

type HydraAPI =
  "hydra" :> HydraCommands

type HydraCommands =
  "init" :> Get '[JSON] A.Value
    :<|> "abort" :> Get '[JSON] A.Value
    :<|> "query" :> "utxo" :> Get '[JSON] A.Value
    :<|> "commit" :> ReqBody '[JSON] CommitUTxOs :> Post '[JSON] A.Value
    :<|> "decommit" :> ReqBody '[JSON] CommitUTxOs :> Post '[JSON] A.Value
    :<|> "protocol-parameters" :> Get '[JSON] A.Value

frameworkErrorHandler :: Either FrameworkError a -> Handler a
frameworkErrorHandler = either toServerError pure
  where
    toServerError :: FrameworkError -> Handler a
    toServerError err =
      throwError $
        err500 {errBody = BSL.fromStrict $ prettyPrintJSON err}

-- Define Handlers
server = initHandler :<|> abortHandler :<|> queryUtxoHandler :<|> commitHandler :<|> decommitHandler :<|> protocolParameterHandler

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

commitHandler :: CommitUTxOs -> Handler A.Value
commitHandler commits = do
  frameworkErrorHandler $ commitUTxO (map T.pack $ utxos commits) (signKey commits)

decommitHandler :: CommitUTxOs -> Handler A.Value
decommitHandler decommits = do
  frameworkErrorHandler $ decommitUTxO (map T.pack $ utxos decommits) (signKey decommits)

protocolParameterHandler :: Handler (A.Value)
protocolParameterHandler = do
  params <- liftIO $ getProtocolParameters
  return params

-- Create API Proxy
deployAPI :: Proxy API
deployAPI = Proxy

-- Define Hydra application
hydraApp :: Application
hydraApp = rewriteRoot (T.pack "index.html") $ static $ cors (const $ Just corsMiddlewarePolicy) $ serve deployAPI server

-- Run the server
main :: IO ()
main = run 8080 hydraApp
