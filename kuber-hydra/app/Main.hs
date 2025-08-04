{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Api.Spec (kuberHydraApp)
import Network.Wai.Handler.Warp (setPort, defaultSettings, setHost, runSettings)
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.Environment
import Websocket.Middleware
import Websocket.SocketConnection
import Cardano.Kuber.Api (chainInfoFromEnv, HasChainQueryAPI (kQueryChainPoint), evaluateKontract)
import Options.Applicative
import Data.String (IsString(..))
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (try, SomeException)
import Cardano.Api (prettyPrintJSON)
import qualified Data.ByteString.Char8 as BS8

data KuberHydraConfig = KuberHydraConfig
  { host     :: Maybe String
  , port     :: Int
  , hydraUrl :: String
  , healthCheck :: Bool
  }

hydraUrlReader :: ReadM String
hydraUrlReader = eitherReader $ \s -> Right s

hydraOpts :: Parser KuberHydraConfig
hydraOpts = KuberHydraConfig
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
      <*>  option hydraUrlReader
          ( long "hydra-url"
         <> help "Url for hydra node websocket connection. Can also be set via HYDRA_URL environment variable."
         <> metavar "URL"
         <> value "" -- Placeholder, actual value derived in main
          )
      <*> switch  (
        long "healthcheck"
        <> help "Perform health-check requests on Cardano and Hydra nodes"
      )

performHealthChecks :: AppConfig -> IO ()
performHealthChecks appConfig = do
  chainTipResult <- try $ evaluateKontract (chainInfo appConfig) kQueryChainPoint
  case chainTipResult of
    Left e -> do
      putStrLn $ "Cardano node connect, Unexpected error : " ++ show (e :: SomeException)
      exitFailure
    Right chainTip -> case chainTip of 
      Left e ->do 
        putStrLn "Cardano Node is not Available"
        print e 
        exitFailure
      Right result -> putStrLn $ "Cardano Node : "  ++ BS8.unpack (prettyPrintJSON result)

  hydraFetch <- fetch appConfig
  hydraHeadStateResult <- try $ hydraFetch "head"
  case hydraHeadStateResult of
    Left e -> do
      putStrLn $ "Hydra head state check FAILED: " ++ show (e :: SomeException)
      exitFailure
    Right response -> do
      putStrLn $ "Hydra head state check PASSED."

main :: IO ()
main = do
  KuberHydraConfig hostStr serverPort hydraUrlFromCli doHealthCheck <- execParser (info (hydraOpts <**> helper)
    ( fullDesc
    <> progDesc "Kuber Hydra Server"
    ))

  hydraUrlFinal <- if null hydraUrlFromCli
    then do
      mHydraUrlEnv <- lookupEnv "HYDRA_URL"
      case mHydraUrlEnv of
        Just url -> return url
        Nothing -> do
          putStrLn "Error: Hydra URL not provided via command line (--hydra-url) or HYDRA_URL environment variable."
          exitFailure
    else return hydraUrlFromCli

  chainInfo <- chainInfoFromEnv
  let appConfig = AppConfig hydraUrlFinal hostStr serverPort chainInfo

  -- Always perform health checks before starting the server
  performHealthChecks appConfig

  if doHealthCheck
    then do
      exitSuccess
    else do
      putStrLn $ "Starting HTTP and WebSocket server on port " ++ show serverPort
      let noCacheApp = noCacheMiddleware (kuberHydraApp appConfig)
      let settings = setPort serverPort defaultSettings
      let finalSettings  = (case hostStr of
            Nothing -> settings
            Just s -> setHost (fromString s) settings  )
      runSettings finalSettings $ websocketsOr WS.defaultConnectionOptions (proxyServer appConfig) noCacheApp
