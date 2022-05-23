{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Kuber.Utility.ChainInfoUtil where
import Cardano.Kuber.Core.ChainInfo ( ChainConnectInfo(..) )
import Cardano.Api (NetworkId(Mainnet, Testnet), NetworkMagic (NetworkMagic), LocalNodeConnectInfo, CardanoMode)
import Control.Exception (try)
import Cardano.Kuber.Util ( localNodeConnInfo )
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Data.Char (toLower)
import System.FilePath (joinPath)

chainInfoMainnet :: IO ChainConnectInfo
chainInfoMainnet =  do 
  conn <-getDefaultConnection "mainnet" Mainnet
  pure $ ChainConnectInfo conn 

chainInfoTestnet :: IO ChainConnectInfo
chainInfoTestnet = do
  let network=Testnet  (NetworkMagic 1097911063)
  conn <-getDefaultConnection  "testnet" network
  pure $ ChainConnectInfo conn 

chainInfoFromEnv :: IO ChainConnectInfo
chainInfoFromEnv = chainInfoFromEnv' "NETWORK"


chainInfoFromEnv' :: String -> IO ChainConnectInfo
chainInfoFromEnv' envKey = do 
  v <- getNetworkFromEnv envKey
  case v of 
    Mainnet -> chainInfoMainnet
    (Testnet  (NetworkMagic 1097911063)) -> chainInfoTestnet
    net ->  do 
      conn <- getDefaultConnection "" net
      pure $ ChainConnectInfo conn  


getDefaultConnection :: String -> NetworkId ->  IO (LocalNodeConnectInfo CardanoMode)
getDefaultConnection networkName networkId= do
  sockEnv <- try $ getEnv "CARDANO_NODE_SOCKET_PATH"
  socketPath <-case  sockEnv of
    Left (e::IOError) -> do
          defaultSockPath<- getWorkPath ( if null networkName then ["node.socket"] else [networkName,"node.socket"])
          exists<-doesFileExist defaultSockPath
          if exists then return defaultSockPath else  (error $ "Socket File is Missing: "++defaultSockPath ++"\n\tSet environment variable CARDANO_NODE_SOCKET_PATH  to use different path")
    Right s -> pure s
  pure (localNodeConnInfo networkId socketPath )

getNetworkFromEnv :: String -> IO NetworkId
getNetworkFromEnv envKey =  do
  networkEnv <- try $ getEnv envKey
  case  networkEnv of
    Left (e::IOError) -> do
          pure (Testnet  (NetworkMagic 1097911063))
    Right s ->  case map toLower s of
      "mainnet" -> pure  Mainnet
      "testnet" -> pure $ Testnet  (NetworkMagic 1097911063)
      _  -> case read s of
        Just v -> pure (Testnet  (NetworkMagic v))
        _ -> fail "Invalid network id"

getWorkPath :: [FilePath] -> IO  FilePath
getWorkPath paths= do
  f <- getWorkPathFunc
  pure $ f paths

getWorkPathFunc :: IO( [FilePath] -> FilePath )
getWorkPathFunc = do
  eitherHome <-try $ getEnv "HOME"
  eitherCardanoHome <- try $ getEnv "CARDANO_HOME"
  case eitherCardanoHome of
    Left (e::IOError) ->   case eitherHome of
        Left (e::IOError) -> error "Can't get Home directory. Missing   HOME and CARDANO_HOME"
        Right home -> pure $ f [home,".cardano"]
    Right home ->  pure $ f  [home]
    where
      f a b = joinPath $ a ++ b
