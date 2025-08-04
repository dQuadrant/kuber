{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTs #-}
module Kuber.Server.Helpers

where
import Network.HTTP.Simple (httpLBS, HttpException (HttpExceptionRequest))
import Network.HTTP.Client.Conduit (Response(responseStatus, responseBody), HttpException (HttpExceptionRequest, InvalidUrlException), Request (requestBody))
import Cardano.Api
import System.Environment (lookupEnv)
import Cardano.Kuber.Api
import Control.Concurrent (threadDelay)
import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Types (status200)
import Data.String (IsString(..))
import Data.Char (toLower)
import Kuber.Server.Spec (appWithBackenAndEra)
import Control.Exception (catch)


appWithEra dcinfo = do
    -- Try to lookup the environment variable
    maybeEra <- lookupEnv "CARDANO_ERA"
    (AnyCardanoEra nodeEra) <- queryNodeEra dcinfo
    eraStr <- case nodeEra of
              BabbageEra -> do
                putStrLn "Connected to Node at Babbage era"
                pure "conway"
              ConwayEra -> do
                 putStrLn "Connected to Node at Conway era"
                 pure "conway"
              era -> do
                putStrLn $ "Node is at " ++ show era ++" Kuber will start in Conway era"
                pure "conway"
    era <- case maybeEra of
          Nothing -> pure eraStr
          Just  era -> do
            putStrLn$  "Starting Kuber with " ++  era ++" era support"
            pure era

    pure $ case map toLower era of
      "conway" -> appWithBackenAndEra dcinfo BabbageEraOnwardsConway
      "babbage" -> appWithBackenAndEra dcinfo BabbageEraOnwardsBabbage
      _ -> error $ "Invalid value of era : " ++ era

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

queryNodeEra :: ChainConnectInfo -> IO AnyCardanoEra
queryNodeEra cinfo = do
  era <- evaluateKontract  cinfo  kQueryCurrentEra
  case era of
    Left fe -> do
      case fe of
        FrameworkError et s ->  putStrLn $ show   et ++ " -> " ++ s
        FrameworkErrors fes -> print fes
      putStrLn "Retrying Node connection in 10 seconds ..."
      threadDelay 10_000_000
      queryNodeEra cinfo
    Right ace ->pure ace