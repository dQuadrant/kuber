{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Spec (hydraApp)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import System.Environment
import Websocket.Aeson
import Websocket.Middleware
import Websocket.SocketConnection

main :: IO ()
main = do
  putStrLn $ "Starting HTTP and WebSocket server on port " ++ show serverPort
  ip <- getEnv "HYDRA_IP"
  port <- getEnv "HYDRA_PORT"
  let host = Host ip (read port)
      noCacheApp = noCacheMiddleware (hydraApp host)
  run serverPort $ websocketsOr WS.defaultConnectionOptions (proxyServer host) noCacheApp