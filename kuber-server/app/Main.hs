{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTs #-}
module Main where


import Network.Wai.Handler.Warp (run, setPort, defaultSettings, setHost, runSettings)
import Kuber.Server.Spec (appWithBackenAndEra)
import Cardano.Kuber.Api (chainInfoFromEnv, throwFrameworkError, ChainConnectInfo, HasChainQueryAPI (kQueryChainPoint), HasCardanoQueryApi(kQueryCurrentEra), evaluateKontract, FrameworkError (..))
import System.Environment (getArgs, lookupEnv)
import Cardano.Kuber.Util (timestampToSlot)
import Data.Text (stripStart)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Read (readMaybe)
import Data.String (IsString(..))
import System.IO
import qualified Data.ByteString.Lazy.Char8 as L8

import Kuber.Server.Helpers
import System.Exit (exitFailure)
import Network.HTTP.Types (status200)
import Control.Exception (try, catch)
import Data.Function ((&))


import Options.Applicative
import Data.Semigroup ((<>))
import Cardano.Api (BabbageEraOnwards (BabbageEraOnwardsBabbage, BabbageEraOnwardsConway), AnyCardanoEra (AnyCardanoEra), CardanoEra (..), ConwayEraOnwards (ConwayEraOnwardsConway))
import Control.Concurrent (threadDelay)
import Data.Char (toLower, toTitle)
import Data.Maybe (fromMaybe)

data KuberConfig = KuberConfig
  { host  :: Maybe String
  , port  :: Int
  , healthCheckUrl :: String
  , healthCheck :: Bool
  }

sample :: Parser KuberConfig
sample = KuberConfig
      <$> option auto (
            long "host"
        <>  short 'H'
        <>  metavar "IP-Address"
        <>  help "IP Address to bind to"
        <>  showDefaultWith (const "Listen on all available intefaces")
        <>  value Nothing
      )
      <*> option auto
          ( long "port"
         <>   short 'p'
         <>   help "Port to listen on"
         <>   showDefault
         <>   value 8081)
      <*>  option auto
          ( long "url"
         <> help "Url for  health-check operation"
         <> showDefaultWith (const "http://127.0.0.1:8081/api/v3/chain-point")
         <> value "http://127.0.0.1:8081/api/v3/chain-point"
         <> metavar "URL" )
      <*> switch  (
        long "healthcheck"
        <> help "Perform health-check request on kuber server"
      )

opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Kuber Server"
  )
main :: IO ()
main = do

  KuberConfig hostStr port healthCheckUrl doHealthCheck <- execParser  opts

  if  doHealthCheck
    then
     performRequest healthCheckUrl

    else do
      -- enable line buffering for instantaneous logs when kuber-server is run in docker container
      hSetBuffering stdout LineBuffering
      dcinfo <- chainInfoFromEnv
      let settings = setPort port defaultSettings
      let settings2  = (case hostStr of
            Nothing -> settings
            Just s -> setHost (fromString s) settings  )
      app <- appWithEra dcinfo

      putStrLn $ "Server started listening on port " ++ show port ++ "."

      runSettings settings2 app



defaultEra :: String
defaultEra = "Conway"
