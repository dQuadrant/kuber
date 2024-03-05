{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Kuber.Server.Spec where

import Cardano.Api
import Cardano.Api.Shelley ( ProtocolParameters, LedgerProtocolParameters)
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
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Typeable (Proxy (..))
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

import Cardano.Kuber.Util
import Kuber.Server.Core
import qualified Data.Aeson as Aeson
import Cardano.Kuber.Data.Parsers
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Kuber.Server.Model
import Control.Monad (liftM)
import Cardano.Kuber.Http.Spec
import qualified Cardano.Ledger.Crypto
import qualified Cardano.Api.Ledger as Cardano.Ledger.Core.Era
import qualified Cardano.Api.Shelley as Cardano.Api.Eon.ShelleyBasedEra


type KubeServer era =  Throws FrameworkError :>  KuberServerApi era

type KuberServerApi_ era =
              -- "api" :>"v3"  :>    QueryApi
         "api" :> "v1"  :> (
                        KuberApi era
                  :<|>  UtilityApi
    )

kuberApiServer queryEra a = (
          queryServer queryEra a
    :<|>  kuberServer a
    :<|>  utilityServer a
  )


queryServer queryEra a =
        makeHandler  a (queryPparamHandler queryEra)
  :<|>  makeHandler a (kQueryChainPoint <&> ChainPointModal)
  :<|>  makeHandler a (kQueryCurrentEra <&> AnyCardanoEraModal)
  :<|> makeHandler2  a (queryUtxosHandler queryEra)
  :<|> (makeHandler a kQuerySystemStart <&> SystemStartModal)
  :<|> (makeHandler a kQueryGenesisParams <&> GenesisParamModal)


kuberServer a =
        makeHandler2 a txBuilderHandler
  :<|>  makeHandler1 a submitTxHandler
  :<|>  makeHandler a queryTimeHandler
  :<|>  makeHandler1 a translatePosixTimeHandler
  :<|>  makeHandler1 a translateSlotHandler

utilityServer a =
          ( makeHandler1 a calculateMinFeeHandler)
    :<|>  ( makeHandler1 a calculateExUnitsHandler)


corsMiddlewarePolicy = CorsResourcePolicy {
      corsOrigins = Nothing
    , corsMethods = [BS.pack "GET",BS.pack "POST",BS.pack "OPTIONS"]
    , corsRequestHeaders = [ fromString "content-type",fromString "api-key"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Just 3600
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = True
    }

appWithBackenAndEra :: (HasChainQueryAPI a, 
    HasLocalNodeAPI a, 
    HasSubmitApi a,
    HasKuberAPI a
    ) => a -> BabbageEraOnwards era-> Application
appWithBackenAndEra dcinfo beraonward = rewriteRoot (T.pack "index.html") $ static $ cors (\r ->  Just corsMiddlewarePolicy ) $ case beraonward of
  BabbageEraOnwardsBabbage -> serve @(KubeServer BabbageEra) Proxy  $ kuberApiServer BabbageEraOnwardsBabbage dcinfo
  BabbageEraOnwardsConway ->  serve @(KubeServer ConwayEra) Proxy  $ kuberApiServer  BabbageEraOnwardsConway dcinfo

instance ToServantErr FrameworkError where
  status (FrameworkError _ _) = status400
  status (FrameworkErrors _) = status400

instance MimeRender PlainText FrameworkError where
  mimeRender ct = mimeRender ct . show
