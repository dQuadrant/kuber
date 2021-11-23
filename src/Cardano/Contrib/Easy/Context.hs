module Cardano.Contrib.Easy.Context
where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Slotting.Time
import Cardano.Contrib.Easy.Util
    ( getDefaultConnection,
      queryProtocolParam,
      querySystemStart,
      queryEraHistory, getNetworkFromEnv )
import Data.Functor ((<&>))
class IsNetworkCtx v where
  toNetworkContext :: v-> IO NetworkContext
  toFullNetworkContext :: v->IO FullNetworkContext
  networkCtxConn :: v-> LocalNodeConnectInfo CardanoMode

  networkCtxNetwork :: v -> NetworkId
  networkCtxNetwork v = localNodeNetworkId $ networkCtxConn v 

newtype MinimalNetworkContext= MinimalNetworkContext (LocalNodeConnectInfo CardanoMode)
 
instance IsNetworkCtx MinimalNetworkContext where
  toNetworkContext  (MinimalNetworkContext conn )=
        queryProtocolParam conn <&> NetworkContext conn 
  toFullNetworkContext  (MinimalNetworkContext conn ) = do
        eHistory <-queryEraHistory conn
        systemStart <-querySystemStart conn
        protocolPrams <-queryProtocolParam conn
        pure  $ FullNetworkContext conn  protocolPrams  systemStart eHistory
  networkCtxConn (MinimalNetworkContext conn )= conn 
  networkCtxNetwork (MinimalNetworkContext conn )=localNodeNetworkId conn 

data  NetworkContext=NetworkContext {
      ctxConn :: LocalNodeConnectInfo CardanoMode,
      ctxProtocolParameters:: ProtocolParameters
    }
instance IsNetworkCtx NetworkContext where
  toNetworkContext  = pure
  toFullNetworkContext  (NetworkContext conn  pParams) = do
        eHistory <-queryEraHistory conn
        systemStart <-querySystemStart conn
        pure  $ FullNetworkContext conn  pParams  systemStart eHistory
  networkCtxConn (NetworkContext conn  _)= conn 

data  FullNetworkContext=FullNetworkContext  {
      fctxConn :: LocalNodeConnectInfo CardanoMode,
      fctxProtocolParameters:: ProtocolParameters,
      fctxSystemStart :: SystemStart,
      fctxEraHistory :: EraHistory CardanoMode
    }

instance IsNetworkCtx FullNetworkContext where
  toNetworkContext  (FullNetworkContext conn  pParam _ _ ) = pure $ NetworkContext conn  pParam
  toFullNetworkContext =pure
  networkCtxConn (FullNetworkContext conn _  _ _)= conn 


getDefaultMainnetContext :: IO MinimalNetworkContext
getDefaultMainnetContext =  do 
  conn <-getDefaultConnection "mainnet" Mainnet
  pure $ MinimalNetworkContext conn 

getDefaultTestnetContext :: IO MinimalNetworkContext
getDefaultTestnetContext = do
  let network=Testnet  (NetworkMagic 1097911063)
  conn <-getDefaultConnection  "testnet" network
  pure $ MinimalNetworkContext conn 

readContextFromEnv :: IO MinimalNetworkContext
readContextFromEnv = readContextFromEnv' "NETWORK"

readContextFromEnv' :: String -> IO MinimalNetworkContext
readContextFromEnv' envKey = do 
  v <- getNetworkFromEnv envKey
  case v of 
    Mainnet -> getDefaultMainnetContext
    (Testnet  (NetworkMagic 1097911063)) -> getDefaultTestnetContext
    net ->  do 
      conn <- getDefaultConnection "" net
      pure $ MinimalNetworkContext conn  
