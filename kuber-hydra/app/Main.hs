{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Spec (hydraApp)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Websocket.Middleware
import Websocket.SocketConnection

main :: IO ()
main = do
  putStrLn $ "Starting HTTP and WebSocket server on port " ++ show serverPort
  ip <- hydraIP
  port <- hydraPort
  let noCacheApp = noCacheMiddleware hydraApp
  run serverPort $ websocketsOr WS.defaultConnectionOptions (proxyServer ip (read port)) noCacheApp