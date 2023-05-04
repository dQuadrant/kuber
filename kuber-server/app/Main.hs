{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeApplications #-}
module Main where


import Network.Wai.Handler.Warp (run, setPort, defaultSettings, setHost, runSettings)
import Kuber.Server.Spec (app)
import Cardano.Kuber.Api (chainInfoFromEnv, throwFrameworkError)
import System.Environment (getArgs)
import Cardano.Kuber.Util (timestampToSlot)
import Data.Text (stripStart)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Read (readMaybe)
import Data.String (IsString(..))
import System.IO
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple (httpLBS, HttpException (HttpExceptionRequest))
import Network.HTTP.Client.Conduit (Response(responseStatus, responseBody), HttpException (HttpExceptionRequest, InvalidUrlException), Request (requestBody))
import System.Exit (exitFailure)
import Network.HTTP.Types (status200)
import Control.Exception (try, catch)
import Data.Function ((&))


import Options.Applicative
import Data.Semigroup ((<>))

data KuberConfig = KuberConfig
  { host  :: Maybe String
  , port  :: Int
  , healthCheckUrl :: String
  , healthCheck :: Bool 
  }

sample :: Parser KuberConfig
sample = KuberConfig
      <$> option auto(     
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
         <> showDefaultWith (const "http://127.0.0.1:8081/api/v1/chaintip")
         <> value "http://127.0.0.1:8081/api/v1/chaintip"
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
      dcinfo <- chainInfoFromEnv  

      let settings = setPort port defaultSettings
      let settings2  = (case hostStr of
            Nothing -> settings
            Just s -> setHost (fromString s) settings  ) 
      putStrLn $ "Starting server on port " ++ show port ++"..."
      runSettings settings2 $ app dcinfo
      run port $ app dcinfo

performRequest :: String -> IO ()
performRequest  url = do
  res <- catch  (httpLBS (fromString url))  exceptionHandler
  if  responseStatus res /=  status200
    then do
      putStr $  "Response " ++ show (responseStatus res) ++" : "
      L8.putStr  $ responseBody res
      exitFailure
    else L8.putStr  $ responseBody res
  where
    exceptionHandler :: HttpException -> IO a
    exceptionHandler ex = do
      case ex of
        HttpExceptionRequest re hec -> putStr  (url ++": " ++  show hec)
        InvalidUrlException s str -> putStr  $  str ++ ": " ++ s
      exitFailure