{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Api.Spec where

import Cardano.Api
import Cardano.Kuber.Api
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String
import qualified Data.Text as T hiding (map)
import GHC.Generics
import Network.HTTP.Types (status201, status400)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Rewrite
import Network.Wai.Middleware.Static
import Servant
import Servant.Exception
import Websocket.Commands
import Websocket.Middleware
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
  "init" :> UVerb 'GET '[JSON] UVerbResponseTypes
    :<|> "abort" :> UVerb 'GET '[JSON] UVerbResponseTypes
    :<|> "query" :> "utxo" :> UVerb 'GET '[JSON] UVerbResponseTypes
    :<|> "commit" :> ReqBody '[JSON] CommitUTxOs :> UVerb 'POST '[JSON] UVerbResponseTypes
    :<|> "decommit" :> ReqBody '[JSON] CommitUTxOs :> UVerb 'POST '[JSON] UVerbResponseTypes
    :<|> "close" :> UVerb 'GET '[JSON] UVerbResponseTypes
    :<|> "contest" :> UVerb 'GET '[JSON] UVerbResponseTypes
    :<|> "fanout" :> UVerb 'GET '[JSON] UVerbResponseTypes
    :<|> "protocol-parameters" :> UVerb 'GET '[JSON] UVerbResponseTypes

frameworkErrorHandler valueOrFe = case valueOrFe of
  Left fe -> throwError $ err500 {errBody = BSL.fromStrict $ prettyPrintJSON fe}
  Right val -> respond $ WithStatus @200 val

toServerError :: FrameworkError -> Handler a
toServerError err =
  throwError $
    err500 {errBody = BSL.fromStrict $ prettyPrintJSON err}

hydraErrorHandler (msg, status) = do
  let jsonResponseOrError = textToJSON msg
  case jsonResponseOrError of
    Left fe -> toServerError fe
    Right jsonResponse ->
      case status of
        200 -> respond $ WithStatus @200 jsonResponse
        201 -> respond $ WithStatus @201 jsonResponse
        _ ->
          throwError $
            (errorMiddleware status)
              { errHTTPCode = status,
                errReasonPhrase = "",
                errBody = A.encode jsonResponse,
                errHeaders = [("Content-Type", "application/json")]
              }

-- Define Handlers
server =
  initHandler
    :<|> abortHandler
    :<|> queryUtxoHandler
    :<|> commitHandler
    :<|> decommitHandler
    :<|> closeHandler
    :<|> contestHandler
    :<|> fanoutHandler
    :<|> protocolParameterHandler

initHandler :: Handler (Union UVerbResponseTypes)
initHandler = do
  initResponse <- liftIO initialize
  hydraErrorHandler initResponse

abortHandler :: Handler (Union UVerbResponseTypes)
abortHandler = do
  abortResponse <- liftIO abort
  hydraErrorHandler abortResponse

queryUtxoHandler :: Handler (Union UVerbResponseTypes)
queryUtxoHandler = do
  queryUtxoResponse <- liftIO queryUTxO
  hydraErrorHandler queryUtxoResponse

commitHandler :: CommitUTxOs -> Handler (Union UVerbResponseTypes)
commitHandler commits = do
  commitResult <- liftIO $ commitUTxO (map T.pack $ utxos commits) (signKey commits)
  frameworkErrorHandler commitResult

decommitHandler :: CommitUTxOs -> Handler (Union UVerbResponseTypes)
decommitHandler decommits = do
  decommitResult <- liftIO $ decommitUTxO (map T.pack $ utxos decommits) (signKey decommits)
  frameworkErrorHandler decommitResult

closeHandler :: Handler (Union UVerbResponseTypes)
closeHandler = do
  closeResponse <- liftIO close
  hydraErrorHandler closeResponse

contestHandler :: Handler (Union UVerbResponseTypes)
contestHandler = do
  closeResponse <- liftIO contest
  hydraErrorHandler closeResponse

fanoutHandler :: Handler (Union UVerbResponseTypes)
fanoutHandler = do
  fanoutResponse <- liftIO fanout
  hydraErrorHandler fanoutResponse

protocolParameterHandler :: Handler (Union UVerbResponseTypes)
protocolParameterHandler = do
  pParamResponse <- liftIO getProtocolParameters
  frameworkErrorHandler pParamResponse

-- Create API Proxy
deployAPI :: Proxy API
deployAPI = Proxy

-- Define Hydra application
hydraApp :: Application
hydraApp = rewriteRoot (T.pack "index.html") $ static $ cors (const $ Just corsMiddlewarePolicy) $ serve deployAPI server

-- Run the server
main :: IO ()
main = run 8080 hydraApp
