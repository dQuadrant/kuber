{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Kuber.Server.Spec where

import Cardano.Api
import qualified Cardano.Api.Ledger as Cardano.Ledger.Core.Era
import Cardano.Api.Shelley (LedgerProtocolParameters, ProtocolParameters)
import qualified Cardano.Api.Shelley as Cardano.Api.Eon.ShelleyBasedEra
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Http.Spec
import Cardano.Kuber.Util
import Cardano.Ledger.Alonzo.Scripts (ExUnits (ExUnits))
import qualified Cardano.Ledger.Crypto
import Control.Exception
  ( Exception,
    IOException,
    SomeException (SomeException),
    catch,
    throw,
    throwIO,
    try,
  )
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), decode, object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import qualified Data.String as String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Proxy (..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType (UserError))
import Kuber.Server.Core
import Kuber.Server.Model
import Kuber.Server.ServantError (HasErrorBody (..), errorMw)
import Network.HTTP.Types
import Network.Wai (Request (requestMethod), Response, ResponseReceived, mapResponseHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCors)
import Network.Wai.Middleware.Rewrite (rewriteRoot)
import Network.Wai.Middleware.Static (static)
import Servant
import qualified Servant.API.ContentTypes as ContentTypes
import Servant.Exception (Exception (..), Throws, ToServantErr (..), mapException)
import Servant.Exception.Server

type KubeServer era = Throws FrameworkError :> KuberServerApi era

type KuberServerApi_ era =
  -- "api" :>"v3"  :>    QueryApi
  "api"
    :> "v1"
    :> ( KuberApi era
           :<|> UtilityApi
       )

kuberApiServer queryEra a =
  ( queryServer queryEra a
      :<|> cardanoServer queryEra a
      :<|> kuberServer a
      :<|> utilityServer a
  )

queryServer queryEra a =
  makeHandler a (queryPparamHandler queryEra)
    :<|> makeHandler a (kQueryChainPoint <&> ChainPointModal)
    :<|> makeHandler2 a (queryUtxosHandler queryEra)

cardanoServer cardanoEra a =
  (makeHandler a kQuerySystemStart <&> SystemStartModal)
    :<|> makeHandler a (kQueryCurrentEra <&> AnyCardanoEraModal)
    :<|> (makeHandler a kQueryGenesisParams <&> GenesisParamModal)
    :<|> (queryHeahtlHandler a)

kuberServer a =
  makeHandler2 a txBuilderHandler
    :<|> makeHandler1 a submitTxHandler
    :<|> makeHandler a queryTimeHandler
    :<|> makeHandler1 a translatePosixTimeHandler
    :<|> makeHandler1 a translateSlotHandler

utilityServer a =
  (makeHandler1 a calculateMinFeeHandler)
    :<|> (makeHandler1 a calculateExUnitsHandler)

corsMiddlewarePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods = [BS.pack "GET", BS.pack "POST", BS.pack "OPTIONS"],
      corsRequestHeaders = [fromString "content-type", fromString "api-key"],
      corsExposedHeaders = Nothing,
      corsMaxAge = Just 3600,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = True
    }

appWithBackenAndEra ::
  ( HasChainQueryAPI a,
    HasCardanoQueryApi a,
    HasLocalNodeAPI a,
    HasSubmitApi a,
    HasKuberAPI a
  ) =>
  a ->
  BabbageEraOnwards era ->
  Application
appWithBackenAndEra dcinfo beraonward = rewriteRoot (T.pack "index.html") $ static $ cors (\r -> Just corsMiddlewarePolicy) $ case beraonward of
  BabbageEraOnwardsBabbage -> serve @(KubeServer BabbageEra) Proxy $ kuberApiServer BabbageEraOnwardsBabbage dcinfo
  BabbageEraOnwardsConway -> serve @(KubeServer ConwayEra) Proxy $ kuberApiServer BabbageEraOnwardsConway dcinfo

instance ToServantErr FrameworkError where
  status (FrameworkError _ _) = status400
  status (FrameworkErrors _) = status400

instance MimeRender PlainText FrameworkError where
  mimeRender ct = mimeRender ct . show
