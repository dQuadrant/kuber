{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server where

import Cardano.Api
import Cardano.Api.Shelley (AlonzoEra)
import Cardano.Contrib.Kubær.Error (ErrorType (..), FrameworkError (..))
import Cardano.Contrib.Kubær.Models
import Cardano.Contrib.Kubær.TxBuilder (TxBuilder)
import Cardano.Contrib.Kubær.Util (executeSubmitTx)
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
import Cardano.Contrib.Kubær.TxBuilder (TxBuilder)
import Cardano.Contrib.Kubær.ChainInfo (DetailedChainInfo(DetailedChainInfo))
import qualified Data.String as String

type TransactionAPI =
  Throws FrameworkError
    :> ( "api" :> "v1" :> "tx" :> ReqBody '[JSON] TxBuilder :> Post '[JSON] (TxResponse )
       )

server :: DetailedChainInfo -> Server TransactionAPI
server dcInfo =
  errorGuard $ txBuilder dcInfo
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

app :: DetailedChainInfo ->  Application
app dcinfo = serve proxyAPI $ server dcinfo

instance ToServantErr FrameworkError where
  status (FrameworkError _ _) = status400
  status (FrameworkErrors _) = status400

instance MimeRender PlainText FrameworkError where
  mimeRender ct = mimeRender ct . show