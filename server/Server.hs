{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server where

import Cardano.Api
import Cardano.Api.Shelley (AlonzoEra)
import Cardano.Contrib.Easy.Context
import Cardano.Contrib.Easy.Error (SomeError (SomeError))
import Cardano.Contrib.Easy.Models
import Cardano.Contrib.Easy.Util (executeSubmitTx)
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
import Core
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), decode, object)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LBS
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
import Network.Wai.Middleware.Servant.Errors (HasErrorBody (..), errorMw)
import Servant
import Servant.Exception (Exception (..), Throws, ToServantErr (..), mapException)
import Servant.Exception.Server

type HttpAPI =
  Throws SomeError
    :> (
         -- General endpoints
         "api" :> "v1" :> "addresses" :> Capture "address" String :> "balance" :> Get '[JSON] BalanceResponse
           :<|> "api" :> "v1" :> "tx" :> "submit":>ReqBody '[JSON] SubmitTxModal :> Post '[JSON] TxResponse
           :<|> "api" :> "v1" :> "tx" :> "pay" :> ReqBody '[JSON] PayToModel  :> Post '[JSON] String
       )

server :: NetworkContext -> Server HttpAPI
server ctx =
  errorGuard (getBalance ctx)
    :<|> errorGuard (submitTx ctx)
    :<|> errorGuard (payToTx ctx)
  where
    errorGuard f v = liftIO $ do
      errorHandler $ f v

    errorHandler f = do
      result <- try f
      case result of
        Left s@(SomeException e) -> do
          case fromException s of
            Nothing -> do
              print e
              throwIO myerr
              where
                myerr :: SomeError
                myerr = SomeError (show e)
            Just s@(SomeError msg) -> do
              putStrLn msg
              throwIO s
        Right v -> pure v

proxyAPI :: Proxy HttpAPI
proxyAPI = Proxy

app :: NetworkContext -> Application
app ctx = serve proxyAPI $ server ctx

instance ToJSON SomeError where
  toJSON (SomeError e) = object [T.pack "message" .= "SomeError", T.pack "message" .= e]

instance ToServantErr SomeError where
  status (SomeError _) = status400

instance MimeRender PlainText SomeError where
  mimeRender ct = mimeRender ct . show