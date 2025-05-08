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
import Cardano.Kuber.Data.Models (TxModal (TxModal))
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
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
import Websocket.TxBuilder (queryUTxO, rawBuildHydraTx, toValidHydraTxBuilder)
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

type GetResp = UVerb 'GET '[JSON] UVerbResponseTypes

type PostResp = UVerb 'POST '[JSON] UVerbResponseTypes

type WithWait sub = QueryParam "wait" Bool :> sub

type API =
  "hydra" :> HydraCommandAPI
    :<|> "hydra" :> "query" :> HydraQueryAPI

type HydraCommandAPI =
  "init" :> WithWait GetResp
    :<|> "abort" :> WithWait GetResp
    :<|> "commit" :> ReqBody '[JSON] CommitUTxOs :> PostResp
    :<|> "decommit" :> WithWait (ReqBody '[JSON] CommitUTxOs :> PostResp)
    :<|> "close" :> WithWait GetResp
    :<|> "contest" :> WithWait GetResp
    :<|> "fanout" :> WithWait GetResp
    :<|> "tx" :> ReqBody '[JSON] TxBuilder :> Post '[JSON] TxModal
    :<|> "submit" :> ReqBody '[JSON] TxModal :> PostResp

type HydraQueryAPI =
  "utxo" :> QueryParam "address" T.Text :> QueryParam "txin" T.Text :> GetResp
    :<|> "protocol-parameters" :> GetResp
    :<|> "state" :> GetResp

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
  commandServer
    :<|> queryServer
  where
    -- Commands: POSTs and state-changing GETs
    commandServer :: Server HydraCommandAPI
    commandServer =
      initHandler
        :<|> abortHandler
        :<|> commitHandler
        :<|> decommitHandler
        :<|> closeHandler
        :<|> contestHandler
        :<|> fanoutHandler
        :<|> txHandler
        :<|> submitHandler
    -- Queries: GET-only, read-only endpoints
    queryServer :: Server HydraQueryAPI
    queryServer =
      queryUtxoHandler
        :<|> queryProtocolParameterHandler
        :<|> queryStateHandler

initHandler :: Maybe Bool -> Handler (Union UVerbResponseTypes)
initHandler wait = do
  initResponse <- liftIO $ initialize (fromMaybe False wait)
  hydraErrorHandler initResponse

abortHandler :: Maybe Bool -> Handler (Union UVerbResponseTypes)
abortHandler wait = do
  abortResponse <- liftIO $ abort (fromMaybe False wait)
  hydraErrorHandler abortResponse

queryUtxoHandler :: Maybe T.Text -> Maybe T.Text -> Handler (Union UVerbResponseTypes)
queryUtxoHandler address txin = do
  queryUtxoResponse <- liftIO $ queryUTxO address txin
  frameworkErrorHandler queryUtxoResponse

commitHandler :: CommitUTxOs -> Handler (Union UVerbResponseTypes)
commitHandler commits = do
  commitResult <- liftIO $ commitUTxO (map T.pack $ utxos commits) (signKey commits)
  frameworkErrorHandler commitResult

decommitHandler :: Maybe Bool -> CommitUTxOs -> Handler (Union UVerbResponseTypes)
decommitHandler wait decommits = do
  decommitResult <- liftIO $ decommitUTxO (map T.pack $ utxos decommits) (signKey decommits) (fromMaybe False wait)
  frameworkErrorHandler decommitResult

closeHandler :: Maybe Bool -> Handler (Union UVerbResponseTypes)
closeHandler wait = do
  closeResponse <- liftIO $ close (fromMaybe False wait)
  hydraErrorHandler closeResponse

contestHandler :: Maybe Bool -> Handler (Union UVerbResponseTypes)
contestHandler wait = do
  closeResponse <- liftIO $ contest (fromMaybe False wait)
  hydraErrorHandler closeResponse

fanoutHandler :: Maybe Bool -> Handler (Union UVerbResponseTypes)
fanoutHandler wait = do
  fanoutResponse <- liftIO $ fanout (fromMaybe False wait)
  hydraErrorHandler fanoutResponse

queryProtocolParameterHandler :: Handler (Union UVerbResponseTypes)
queryProtocolParameterHandler = do
  pParamResponse <- liftIO getProtocolParameters
  frameworkErrorHandler pParamResponse

queryStateHandler :: Handler (Union UVerbResponseTypes)
queryStateHandler = do
  stateResponse <- liftIO getHydraState
  frameworkErrorHandler (stateResponse :: Either FrameworkError A.Value)

txHandler :: TxBuilder_ ConwayEra -> Handler TxModal
txHandler txb = do
  hydraTxModal <- liftIO $ toValidHydraTxBuilder txb
  case hydraTxModal of
    Left fe -> toServerError fe
    Right txm -> pure txm

submitHandler :: TxModal -> Handler (Union UVerbResponseTypes)
submitHandler txm = do
  submitResponse <- liftIO $ submit txm
  hydraErrorHandler submitResponse

-- Create API Proxy
deployAPI :: Proxy API
deployAPI = Proxy

-- Define Hydra application
hydraApp :: Application
hydraApp = rewriteRoot (T.pack "index.html") $ static $ cors (const $ Just corsMiddlewarePolicy) $ serve deployAPI server

-- Run the server
main :: IO ()
main = run 8080 hydraApp
