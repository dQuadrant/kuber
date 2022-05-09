module Main where


-- import Cardano.Contrib.Kuber.ChainInfo (IsNetworkCtx (toFullNetworkContext, toNetworkContext), getDefaultTestnetContext, readContextFromEnv)
import Network.Wai.Handler.Warp (run)
import Server (app)
import Cardano.Contrib.Kuber.ChainInfo

main = do
  ctx <- readContextFromEnv
  chainInfo <- withDetails ctx
  let port=8081
  putStrLn $ "Starting server on port " ++ show port ++"..."
  run port $ app chainInfo