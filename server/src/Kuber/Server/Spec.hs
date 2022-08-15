{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}

module Kuber.Server.Spec where

import Cardano.Api
import Cardano.Api.Shelley (BabbageEra)
import Cardano.Kuber.Data.Models

import Control.Exception
  ( Exception,
    IOException,
    SomeException (SomeException),
    catch,
    throw,
    throwIO,
    try,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), decode, object)
import qualified Data.ByteString as ByteString
import Data.Data (typeOf)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Typeable (Proxy (..))
import GHC.Conc (atomically, newTVar, newTVarIO)
import GHC.Conc.IO (threadDelay)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType (UserError))
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Kuber.Server.ServantError (HasErrorBody (..), errorMw)
import Servant
import Servant.Exception (Exception (..), Throws, ToServantErr (..), mapException)
import Servant.Exception.Server

import qualified Data.String as String

import qualified Servant.API.ContentTypes as ContentTypes
import Cardano.Ledger.Alonzo.Scripts (ExUnits(ExUnits))
import Data.Text (Text)
import qualified Data.ByteString.Lazy
import Network.Wai.Middleware.Cors (simpleCors, CorsResourcePolicy (..), cors)
import qualified Data.ByteString.Char8 as BS
import Network.Wai (Request(requestMethod), Response, ResponseReceived, mapResponseHeaders)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.Rewrite (rewriteRoot)
import Cardano.Kuber.Api
import Kuber.Server.MediaType

import Cardano.Kuber.Util (evaluateExecutionUnits)
import Kuber.Server.Core
import qualified Data.Aeson as Aeson
import Cardano.Kuber.Data.Parsers
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))

type TransactionAPI =
  Throws FrameworkError
    :> (
            "api" :> "v1" :> "tx" :> QueryParam "submit" Bool :> ReqBody '[JSON] TxBuilder :> Post '[JSON] (TxResponse )
      :<|>  "api" :> "v1" :> "tx" :> "submit" :> ReqBody '[JSON ,CBORBinary,CBORText  ] (SubmitTxModal ) :> Post '[JSON] (TxResponse )
      :<|>  "api" :> "v1" :> "tx" :> "exUnits" :> ReqBody '[CBORText,CBORBinary,CBORText   ] (Tx BabbageEra) :> Post '[JSON] ([Either String ExecutionUnits ])
      :<|> "api" :> "v1" :> "scriptPolicy"        :> ReqBody '[JSON] (Aeson.Value ) :> Post '[PlainText ] (Text)
      :<|> "api" :> "v1" :> "keyhash"        :> ReqBody '[JSON] (AddressModal) :> Post '[JSON ] (KeyHashResponse)

       )

server :: DetailedChainInfo -> Server TransactionAPI
server dcInfo =
   errorGuard2 (txBuilder dcInfo)
  :<|> errorGuard (submitTx' dcInfo)
  :<|> errorGuard (evaluateExecutionUnits dcInfo )
  :<|> errorGuard (\sc -> parseAnyScript (Aeson.encode sc)<&> (\(ScriptInAnyLang sl sc') ->serialiseToRawBytesHexText ( scriptPolicyId sc')) )
  :<|> errorGuard (getKeyHash)
  where

    errorGuard f v = liftIO $ do
      errorHandler $ f v
    errorGuard2 f v1 v2 = liftIO $ do errorHandler $ f v1 v2

    errorHandler f = do
      result <- try f
      case result of
        Left s@(SomeException e) -> do
          case fromException s of
            Nothing -> do
              print e
              throwIO myerr
              where
                myerr :: FrameworkError
                myerr = FrameworkError ParserError (show e)
            Just s@(FrameworkError _ msg) -> do
              putStrLn msg
              throwIO s
            Just s@(FrameworkErrors errs) -> do
              putStrLn $ String.fromString (intercalate "\n" (map (\(FrameworkError _ feMessage)->feMessage) errs))
              throwIO s
        Right v -> pure v

proxyAPI :: Proxy TransactionAPI
proxyAPI = Proxy


-- app :: NetworkContext -> Application
-- app ctx = serve proxyAPI $ server ctx
corsMiddlewarePolicy = CorsResourcePolicy {
         corsOrigins = Nothing
            -- Just( ([BS.pack "http://localhost:3000",
            --   BS.pack "http://localhost:8080",
            --   BS.pack "http://cnftregistry.io",
            --   BS.pack "http://testnet.cnftregistry.io",
            --   BS.pack "http://dev.cnftregistry.io"], True))

    -- | HTTP methods that are allowed in CORS requests.
    --
    , corsMethods = [BS.pack "GET",BS.pack "POST",BS.pack "OPTIONS"]

    -- | Field names of HTTP request headers that are allowed in CORS requests.
    -- Header names that are included in 'simpleHeaders', except for
    -- @content-type@, are implicitly included and thus optional in this list.
    --
    , corsRequestHeaders = [ fromString "content-type"]

    -- | Field names of HTTP headers that are exposed to the client in the response.
    --
    , corsExposedHeaders = Nothing

    -- | Number of seconds that the OPTIONS preflight response may be cached by the client.
    --
    -- Tip: Set this to 'Nothing' while testing your CORS implementation, then increase
    -- it once you deploy to production.
    --
    , corsMaxAge = Just 3600

    -- | If the resource is shared by multiple origins but
    -- @Access-Control-Allow-Origin@ is not set to @*@ this may be set to
    -- 'True' to cause the server to include a @Vary: Origin@ header in the
    -- response, thus indicating that the value of the
    -- @Access-Control-Allow-Origin@ header may vary between different requests
    -- for the same resource. This prevents caching of the responses which may
    -- not apply accross different origins.
    --
    , corsVaryOrigin = False

    -- | If this is 'True' and the request does not include an @Origin@ header
    -- the response has HTTP status 400 (bad request) and the body contains
    -- a short error message.
    --
    -- If this is 'False' and the request does not include an @Origin@ header
    -- the request is passed on unchanged to the application.
    --
    -- @since 0.2
    , corsRequireOrigin = False

    -- | In the case that
    --
    -- * the request contains an @Origin@ header and
    --
    -- * the client does not conform with the CORS protocol
    --   (/request is out of scope/)
    --
    -- then
    --
    -- * the request is passed on unchanged to the application if this field is
    --   'True' or
    --
    -- * an response with HTTP status 400 (bad request) and short
    --   error message is returned if this field is 'False'.
    --
    -- Note: Your application needs to will receive preflight OPTIONS requests if set to 'True'.
    --
    -- @since 0.2
    --
    , corsIgnoreFailures = True
    }
app :: DetailedChainInfo ->  Application
app dcinfo = rewriteRoot (T.pack "index.html") $ static $ cors (\r ->  Just corsMiddlewarePolicy ) $ serve proxyAPI $ server dcinfo

instance ToServantErr FrameworkError where
  status (FrameworkError _ _) = status400
  status (FrameworkErrors _) = status400

instance MimeRender PlainText FrameworkError where
  mimeRender ct = mimeRender ct . show

