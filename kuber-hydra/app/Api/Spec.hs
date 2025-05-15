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
{-# LANGUAGE OverloadedRecordDot #-}
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
import Cardano.Kuber.Data.Models
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.String
import qualified Data.Text as T hiding (map)
import GHC.Generics
import Network.HTTP.Types (status201, status400)
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Rewrite
import Network.Wai.Middleware.Static
import Servant
import Servant.Exception
import Websocket.Aeson
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
    signKey :: Maybe A.Value
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
server appConfig =
  commandServer appConfig
    :<|> queryServer appConfig
  where
    -- Commands: POSTs and state-changing GETs
    commandServer :: AppConfig -> Server HydraCommandAPI
    commandServer appConfig =
      initHandler appConfig
        :<|> abortHandler appConfig
        :<|> commitHandler appConfig
        :<|> decommitHandler appConfig
        :<|> closeHandler appConfig
        :<|> contestHandler appConfig
        :<|> fanoutHandler appConfig
        :<|> txHandler appConfig
        :<|> submitHandler appConfig
    -- Queries: GET-only, read-only endpoints
    queryServer :: AppConfig -> Server HydraQueryAPI
    queryServer appConfig =
      queryUtxoHandler appConfig
        :<|> queryProtocolParameterHandler appConfig
        :<|> queryStateHandler appConfig

initHandler :: AppConfig -> Maybe Bool -> Handler (Union UVerbResponseTypes)
initHandler appConfig wait = do
  initResponse <- liftIO $ initialize appConfig (fromMaybe False wait)
  hydraErrorHandler initResponse

abortHandler :: AppConfig -> Maybe Bool -> Handler (Union UVerbResponseTypes)
abortHandler appConfig wait = do
  abortResponse <- liftIO $ abort appConfig (fromMaybe False wait)
  hydraErrorHandler abortResponse

queryUtxoHandler :: AppConfig -> Maybe T.Text -> Maybe T.Text -> Handler (Union UVerbResponseTypes)
queryUtxoHandler appConfig address txin = do
  queryUtxoResponse <- liftIO $ queryUTxO appConfig address txin
  frameworkErrorHandler queryUtxoResponse

commitHandler :: AppConfig -> CommitUTxOs -> Handler (Union UVerbResponseTypes)
commitHandler appConfig commits = do
  commitResult <- liftIO $ commitUTxO appConfig (map T.pack $ commits.utxos) (signKey commits)
  frameworkErrorHandler commitResult

decommitHandler :: AppConfig -> Maybe Bool -> CommitUTxOs -> Handler (Union UVerbResponseTypes)
decommitHandler appConfig wait decommits = do
  decommitResult <- liftIO $ decommitUTxO appConfig (map T.pack $ decommits.utxos) (signKey decommits) (fromMaybe False wait)
  frameworkErrorHandler decommitResult

closeHandler :: AppConfig -> Maybe Bool -> Handler (Union UVerbResponseTypes)
closeHandler appConfig wait = do
  closeResponse <- liftIO $ close appConfig (fromMaybe False wait)
  hydraErrorHandler closeResponse

contestHandler :: AppConfig -> Maybe Bool -> Handler (Union UVerbResponseTypes)
contestHandler appConfig wait = do
  closeResponse <- liftIO $ contest appConfig (fromMaybe False wait)
  hydraErrorHandler closeResponse

fanoutHandler :: AppConfig -> Maybe Bool -> Handler (Union UVerbResponseTypes)
fanoutHandler appConfig wait = do
  fanoutResponse <- liftIO $ fanout appConfig (fromMaybe False wait)
  hydraErrorHandler fanoutResponse

queryProtocolParameterHandler :: AppConfig -> Handler (Union UVerbResponseTypes)
queryProtocolParameterHandler appConfig = do
  pParamResponse <- liftIO $ getProtocolParameters appConfig
  frameworkErrorHandler pParamResponse

queryStateHandler :: AppConfig -> Handler (Union UVerbResponseTypes)
queryStateHandler appConfig = do
  stateResponse <- liftIO $ getHydraState appConfig
  frameworkErrorHandler (stateResponse :: Either FrameworkError A.Value)

txHandler :: AppConfig -> TxBuilder_ ConwayEra -> Handler TxModal
txHandler appConfig txb = do
  hydraTxModal <- liftIO $ toValidHydraTxBuilder appConfig txb
  case hydraTxModal of
    Left fe -> toServerError fe
    Right txm -> pure txm

submitHandler :: AppConfig -> TxModal -> Handler (Union UVerbResponseTypes)
submitHandler appConfig txm = do
  submitResponse <- liftIO $ submit appConfig txm
  hydraErrorHandler submitResponse

-- Create API Proxy
deployAPI :: Proxy API
deployAPI = Proxy

-- Define Hydra application
hydraApp :: AppConfig -> Application
hydraApp appConfig = rewriteRoot (T.pack "index.html") $ static $ cors (const $ Just corsMiddlewarePolicy) $ serve deployAPI (server appConfig)
