module Kuber.Server.Main where


import Cardano.Kuber.ChainInfo (IsNetworkCtx (toFullNetworkContext, toNetworkContext), getDefaultTestnetContext, readContextFromEnv)
import Network.Wai.Handler.Warp (run)
import Kuber.Server.Spec (app)
import Cardano.Kuber.ChainInfo

main = do
  ctx <- readContextFromEnv
  chainInfo <- withDetails ctx
  let port=8081
  putStrLn $ "Starting server on port " ++ show port ++"..."
  run port $ app chainInfo