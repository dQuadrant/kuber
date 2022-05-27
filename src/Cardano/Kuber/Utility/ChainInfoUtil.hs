{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Kuber.Utility.ChainInfoUtil where
import Cardano.Kuber.Core.ChainInfo ( ChainConnectInfo(..) )
import Cardano.Api (NetworkId(Mainnet, Testnet), NetworkMagic (NetworkMagic), LocalNodeConnectInfo (LocalNodeConnectInfo), CardanoMode, ConsensusModeParams (CardanoModeParams), EpochSlots (EpochSlots))
import Control.Exception (try)
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Data.Char (toLower)
import System.FilePath (joinPath)
import qualified Debug.Trace as Debug
import Text.Read (readMaybe)

-- type wrapper for EnvironmentVariable String
type EnvVariable = String

-- Helper function to get Node conenction information
localNodeConnInfo :: NetworkId -> FilePath   -> LocalNodeConnectInfo CardanoMode
localNodeConnInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 21600))


-- Using Environment variables get mainnet's ConnectInfo
-- If CARDANO_NODE_SOCKET_PATH environment variable is set,  the socket path is set to it's value
-- Otherwise CARDANO_HOME or "$HOME/.cardano"  is used and the socket path becomes "$CARDANO_HOME/.cardano/mainnet/node.socket"
chainInfoMainnet :: IO ChainConnectInfo
chainInfoMainnet =  do
  conn <-getDefaultConnection "mainnet" Mainnet
  pure $ ChainConnectInfo conn

-- Using Environment variables get testnet's ConnectInfo
-- If CARDANO_NODE_SOCKET_PATH environment variable is set,  the socket path is set to it's value
-- Otherwise CARDANO_HOME or "$HOME/.cardano"  is used and the socket path becomes "$CARDANO_HOME/.cardano/testnet/node.socket"
chainInfoTestnet :: IO ChainConnectInfo
chainInfoTestnet = do
  let network=Testnet  (NetworkMagic 1097911063)
  conn <-getDefaultConnection  "testnet" network
  pure $ ChainConnectInfo conn


-- Using Environment variables determine the NETWORK.
-- NETWORK can be "mainnet", "testnet" or "networkMagic number".
-- If CARDANO_NODE_SOCKET_PATH environment variable is set,  the socket path is set to it's value
-- Otherwise CARDANO_HOME or "$HOME/.cardano"  is used and the socket path becomes "$CARDANO_HOME/node.socket"
chainInfoFromEnv :: IO ChainConnectInfo
chainInfoFromEnv = chainInfoFromEnv' "NETWORK"

-- Read Network value from the environment variable and then determine connection info
-- If CARDANO_NODE_SOCKET_PATH environment variable is set,  the socket path is set to it's value
-- Otherwise CARDANO_HOME or "$HOME/.cardano"  is used and the socket path becomes "$CARDANO_HOME/node.socket"
chainInfoFromEnv' :: EnvVariable -> IO ChainConnectInfo
chainInfoFromEnv' envKey = do
  v <- getNetworkFromEnv envKey
  case v of
    Mainnet -> chainInfoMainnet
    (Testnet  (NetworkMagic 1097911063)) -> chainInfoTestnet
    net ->  do
      conn <- getDefaultConnection "" net
      pure $ ChainConnectInfo conn


-- If CARDANO_NODE_SOCKET_PATH environment variable is set,  return ConnectInfo instance with the path
-- Otherwise CARDANO_HOME or "$HOME/.cardano"  is used and the socket path becomes "$CARDANO_HOME/node.socket"
getDefaultConnection :: String -> NetworkId ->  IO (LocalNodeConnectInfo CardanoMode)
getDefaultConnection networkName networkId= do
  sockEnv <- try $ getEnv "CARDANO_NODE_SOCKET_PATH"
  socketPath <-case  sockEnv of
    Left (e::IOError) -> do
          defaultSockPath<- getWorkPath ( if null networkName then ["node.socket"] else [networkName,"node.socket"])
          exists<-doesFileExist defaultSockPath
          if exists then return defaultSockPath else  error $ "Socket File is Missing: "++defaultSockPath ++"\n\tSet environment variable CARDANO_NODE_SOCKET_PATH  to use different path"
    Right s -> pure s
  pure (localNodeConnInfo networkId socketPath )


-- Given environment variable key, read the environmet variable and return network Id
getNetworkFromEnv :: EnvVariable -> IO NetworkId
getNetworkFromEnv envKey =  do
  networkEnv <- try $ getEnv envKey
  case  networkEnv of
    Left (e::IOError) -> do
          pure (Testnet  (NetworkMagic 1097911063))
    Right s ->  case map toLower s of
      "mainnet" -> pure  Mainnet
      "testnet" -> pure $ Testnet  (NetworkMagic 1097911063)
      _  -> do 
        Debug.traceM s
        case readMaybe s of
          Just v -> pure (Testnet  (NetworkMagic  v))
          _ -> fail "Invalid network id : "

-- get absolute path given a directoryor file path.
-- the absolute path is "CARANO_HOME/...paths" value to the path
getWorkPath :: [FilePath] -> IO  FilePath
getWorkPath paths= do
  f <- getWorkPathFunc
  pure $ f paths


-- Get WrokPath calculatin Function
-- getWorkPath function can throw errors. The error is only during initialization
-- So if the function succeeds, it returns pure function to calculate filepath which can be reused.
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
