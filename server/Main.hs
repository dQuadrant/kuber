module Main where


import Network.Wai.Handler.Warp (run)
import Kuber.Server.Spec (app)
import Cardano.Kuber.Api (chainInfoFromEnv, ChainInfo (withDetails))

main :: IO ()
main = do
  dcinfo <- chainInfoFromEnv >>= withDetails

  let port=8081
  putStrLn $ "Starting server on port " ++ show port ++"..."
  run port $ app dcinfo