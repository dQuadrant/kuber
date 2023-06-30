{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Kuber.Server.Model where
import Data.Word (Word64)
import Cardano.Api
import qualified Data.Aeson.Key as A
import Data.Aeson (toJSON,fromJSON, (.!=))
import qualified Data.Aeson as A
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified PlutusLedgerApi.V1 as Plutus
import PlutusLedgerApi.Common (EvaluationError(..))
import Cardano.Kuber.Api (FrameworkError)
import Data.Aeson.Types ((.:?))
import Data.Functor ((<&>))

