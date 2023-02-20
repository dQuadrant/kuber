{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where


import Network.Wai.Handler.Warp (run, setPort, defaultSettings, setHost, runSettings)
import Kuber.Server.Spec (app)
import Cardano.Kuber.Api (chainInfoFromEnv, ChainInfo (withDetails), DetailedChainInfo (DetailedChainInfo))
import System.Environment (getArgs)
import Cardano.Kuber.Util (timestampToSlot)
import Data.Text (stripStart)
import Data.Data (Data)
import Data.Typeable (Typeable)
import System.Console.CmdArgs
import Text.Read (readMaybe)
import Data.String (IsString(..))
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  dcinfo <- chainInfoFromEnv >>= withDetails
  Modes port hostStr <- cmdArgs $ modes [
        Modes {
          port = 8081 &= typ "Port",
          host = "*" &=typ "Host"
        }
      ]&=program "kuber"

  let settings = setPort port defaultSettings
      host = setHost (fromString hostStr) settings
  putStrLn $ "Starting server on port " ++ show port ++"..."
  runSettings host $ app dcinfo
  run port $ app dcinfo

data Modes =
      Modes {
        port:: Int,
        host :: String
      }
       deriving (Show, Data, Typeable)
