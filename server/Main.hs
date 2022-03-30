module Main where

import Cardano.Contrib.Easy.Context (IsNetworkCtx (toFullNetworkContext, toNetworkContext), getDefaultTestnetContext, readContextFromEnv)
import Network.Wai.Handler.Warp (run)
import Server (app)

main = do
  ctx <- readContextFromEnv
  networkCtx <- toNetworkContext ctx
  let port=8081
  putStrLn $ "Starting server on port " ++ show port ++"..."
  run port $ app networkCtx