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
{-# LANGUAGE OverlappingInstances #-}
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
import Data.Maybe
import Data.String
import qualified Data.Text as T hiding (map)
import GHC.Generics
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Rewrite
import Network.Wai.Middleware.Static
import Servant
import Websocket.Aeson
import Websocket.Commands
import Websocket.Middleware
import Websocket.TxBuilder (hydraProtocolParams, queryUTxO, toValidHydraTxBuilder, buildDecommitTx, submitDecommitTx)
import Websocket.Utils
import Websocket.SocketConnection (fetch, AppConfig (chainInfo), getHead, getCommits)
import Servant.Exception (Throws)
import qualified Data.Text.Encoding as T
import qualified  Data.ByteString.Lazy.Char8 as BS8L
import Data.Functor ((<&>))
import Cardano.Kuber.Data.Models (TxModal)
import Cardano.Kuber.Http.Spec (KuberServerApi)
import Kuber.Server.Spec ( corsMiddlewarePolicy, kuberApiServer )
import Cardano.Kuber.Data.Parsers (parseAddress)


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
  { utxos :: [TxIn],
    signKey :: Maybe A.Value
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type GetResp = UVerb 'GET '[JSON] UVerbResponseTypes

type PostResp = UVerb 'POST '[JSON] UVerbResponseTypes

type WithWait sub = QueryParam "wait" Bool :> sub

type WithSubmit sub = QueryParam "submit" Bool :> sub


type ExceptKuberApi = Throws FrameworkError :> KuberServerApi ConwayEra

type KuberHydraApi =
        HydraServerApi
  :<|> ExceptKuberApi

type HydraServerApi =
  "hydra" :> HydraCommandAPI
    :<|> "hydra" :> "query" :> HydraQueryAPI

type HydraCommandAPI =

  "init" :> WithWait PostResp
    :<|> "abort" :> WithWait PostResp
    :<|> "commit" :> WithSubmit (ReqBody '[JSON] CommitUTxOs :> PostResp)
    :<|> "close" :> WithWait PostResp
    :<|> "decommit" :> QueryParams "txin" T.Text :> GetResp -- New GET decommit
    :<|> "decommit" :> ReqBody '[JSON] TxModal :> WithWait PostResp -- New POST decommit
    :<|> "contest" :> WithWait PostResp
    :<|> "fanout" :> WithWait PostResp
    :<|> "tx" :> WithSubmit (ReqBody '[JSON] TxBuilder :> Post '[JSON] TxModal)
    :<|> "submit" :> ReqBody '[JSON] TxModal :> WithWait PostResp

type HydraQueryAPI =

  "utxo" :> QueryParams "address" T.Text :> QueryParams "txin" T.Text :> GetResp
    :<|> "head" :> GetResp
    :<|> "protocol-parameters" :> GetResp
    :<|> "state" :> GetResp
    :<|> "commits" :> GetResp

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

kuberHydraServer :: AppConfig -> Server KuberHydraApi
kuberHydraServer appConfig =
        hydraServer appConfig
  :<|> kuberApiServer BabbageEraOnwardsConway (chainInfo appConfig)


-- Define Handlers
hydraServer :: AppConfig -> Server HydraServerApi
hydraServer appConfig =
  commandServer appConfig
    :<|> queryServer appConfig
  where
    -- Commands: POSTs and state-changing GETs
    commandServer :: AppConfig -> Server HydraCommandAPI
    commandServer appConfig =
      initHandler appConfig
        :<|> abortHandler appConfig
        :<|> commitHandler appConfig
        :<|> closeHandler appConfig
        :<|> decommitQueryTxHandler appConfig
        :<|> decommitSubmitTxModalHandler appConfig
        :<|> contestHandler appConfig
        :<|> fanoutHandler appConfig
        :<|> txHandler appConfig
        :<|> submitHandler appConfig
    -- Queries: GET-only, read-only endpoints
    queryServer :: AppConfig -> Server HydraQueryAPI
    queryServer appConfig =
      queryUtxoHandler appConfig
        :<|> queryHeadlHandler appConfig
        :<|> queryProtocolParameterHandler appConfig
        :<|> queryStateHandler appConfig
        :<|> getCommitsHandler appConfig

initHandler :: AppConfig -> Maybe Bool -> Handler (Union UVerbResponseTypes)
initHandler appConfig wait = do
  initResponse <- liftIO $ initialize appConfig (fromMaybe False wait)
  hydraErrorHandler initResponse

abortHandler :: AppConfig -> Maybe Bool -> Handler (Union UVerbResponseTypes)
abortHandler appConfig wait = do
  abortResponse <- liftIO $ abort appConfig (fromMaybe False wait)
  hydraErrorHandler abortResponse

queryHeadlHandler :: AppConfig -> Handler (Union UVerbResponseTypes)
queryHeadlHandler appConfig=do
    response <- liftIO $ getHead appConfig
    hydraErrorHandler ( T.decodeUtf8 ( BSL.toStrict response),200)

queryUtxoHandler :: AppConfig -> [T.Text] -> [T.Text] -> Handler (Union UVerbResponseTypes)
queryUtxoHandler appConfig address txin = do
  parsedTxIns <- liftIO $ listOfTextToTxIn txin
  parsedAddresses <- liftIO $ mapM (parseAddress @ConwayEra) address

  eitherErrorOrUTxOs <- case parsedTxIns of
    Left fe -> pure $ Left fe
    Right txins -> do
        queryUtxoResponse <- liftIO $ queryUTxO appConfig parsedAddresses txins
        case queryUtxoResponse of
          Left fe -> pure $ Left fe
          Right utxos -> case bytestringToJSON $ BSL.fromStrict $ serialiseToJSON utxos of
            Left fe' -> pure $ Left fe'
            Right json -> pure $ Right json
  frameworkErrorHandler eitherErrorOrUTxOs

commitHandler :: AppConfig -> Maybe Bool -> CommitUTxOs -> Handler (Union UVerbResponseTypes)
commitHandler appConfig submit commits = do
  commitResult <- liftIO $ commitUTxO appConfig commits.utxos (signKey commits) (fromMaybe False submit)
  commitResultToJSON <- liftIO $ eitherObjectToJSON commitResult
  frameworkErrorHandler commitResultToJSON


getCommitsHandler :: AppConfig ->  Handler (Union UVerbResponseTypes)
getCommitsHandler appConfig  = do
  commitsResult <- liftIO $ getCommits appConfig
  let result = case A.eitherDecode commitsResult of
                  Left err ->  Left $ FrameworkError ParserError ( BS8L.unpack commitsResult)
                  Right (v ::  A.Value) -> Right v
  frameworkErrorHandler result

decommitSubmitTxModalHandler :: AppConfig -> TxModal -> Maybe Bool -> Handler (Union UVerbResponseTypes)
decommitSubmitTxModalHandler appConfig txm wait = do
  result <- liftIO (submitDecommitTx appConfig txm (fromMaybe False wait ))
  hydraErrorHandler result


decommitQueryTxHandler :: AppConfig -> [T.Text] -> Handler (Union UVerbResponseTypes)
decommitQueryTxHandler appConfig txin = do
  parsedTxIns <- liftIO $ listOfTextToTxIn txin
  eitherErrorOrTxModal <- case parsedTxIns of
    Left fe -> pure $ Left fe
    Right txins -> liftIO (buildDecommitTx appConfig txins) <&> fmap A.toJSON
  frameworkErrorHandler eitherErrorOrTxModal
  -- case eitherErrorOrTxModal of
  --   Left fe -> frameworkErrorHandler (Left fe)
  --   Right txModal -> frameworkErrorHandler (Right $ A.toJSON txModal)

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
  pParamResponse <- liftIO (hydraProtocolParams appConfig :: IO (Either FrameworkError HydraProtocolParameters))
  pParamsToJSON <- liftIO $ eitherObjectToJSON pParamResponse
  frameworkErrorHandler pParamsToJSON

queryStateHandler :: AppConfig -> Handler (Union UVerbResponseTypes)
queryStateHandler appConfig = do
  stateResponse <- liftIO $ getHydraState appConfig
  stateResponseJSON <- liftIO $ eitherObjectToJSON stateResponse
  frameworkErrorHandler stateResponseJSON

txHandler :: AppConfig -> Maybe Bool -> TxBuilder_ ConwayEra -> Handler TxModal
txHandler appConfig submit txb = do
  hydraTxModal <- liftIO $ toValidHydraTxBuilder appConfig txb (fromMaybe False submit)
  case hydraTxModal of
    Left fe -> toServerError fe
    Right txm -> pure txm

submitHandler :: AppConfig -> TxModal -> Maybe Bool -> Handler (Union UVerbResponseTypes)
submitHandler appConfig txm wait = do
  submitResponse <- liftIO $ submit appConfig txm (fromMaybe False wait)
  submitResponseJSON <- liftIO $ eitherObjectToJSON submitResponse
  frameworkErrorHandler submitResponseJSON

-- Create API Proxy
hydraServerApiProxy :: Proxy KuberHydraApi
hydraServerApiProxy = Proxy

-- Define Hydra application
kuberHydraApp :: AppConfig -> Application
kuberHydraApp appConfig = rewriteRoot (T.pack "index.html") $ static $ cors (const $ Just Kuber.Server.Spec.corsMiddlewarePolicy) $ serve hydraServerApiProxy (kuberHydraServer appConfig)
