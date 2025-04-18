{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Spec (hydraApp)
import  Websocket.SocketConnection
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.RequestLogger
import qualified Network.WebSockets as WS
import System.Environment (getEnv)


-- Main Function: Runs both HTTP & WebSocket on the same port
main :: IO ()
main = do
  putStrLn $ "Starting HTTP and WebSocket server on port " ++ show serverPort
  ip <- hydraIP
  port <- hydraPort
  run serverPort $ websocketsOr WS.defaultConnectionOptions (proxyServer ip (read port)) hydraApp