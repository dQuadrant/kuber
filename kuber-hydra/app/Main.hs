{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Spec (hydraApp)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Websocket.SocketConnection

main :: IO ()
main = do
  putStrLn $ "Starting HTTP and WebSocket server on port " ++ show serverPort
  ip <- hydraIP
  port <- hydraPort
  run serverPort $ websocketsOr WS.defaultConnectionOptions (proxyServer ip (read port)) hydraApp