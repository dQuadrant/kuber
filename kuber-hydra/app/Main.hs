{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Api.Spec (hydraApp)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.Environment
import Websocket.Aeson
import Websocket.Middleware
import Websocket.SocketConnection
import Configuration.Dotenv (loadFile, defaultConfig)

main :: IO ()
main = do
  loadFile defaultConfig
  hydraIp <- getEnv "HYDRA_IP"
  hydraPort <- getEnv "HYDRA_PORT"
  serverPort <- getEnv "SERVER_PORT"
  putStrLn $ "Starting HTTP and WebSocket server on port " ++ show serverPort
  let host = AppConfig hydraIp (read hydraPort) "0.0.0.0" (read serverPort)
      noCacheApp = noCacheMiddleware (hydraApp host)
  run (read serverPort) $ websocketsOr WS.defaultConnectionOptions (proxyServer host) noCacheApp