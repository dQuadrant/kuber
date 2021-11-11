module Cardano.Contrib.Easy.Context
where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Slotting.Time
import Cardano.Contrib.Easy.Util
    ( getDefaultConnection,
      queryProtocolParam,
      querySystemStart,
      queryEraHistory )
import Data.Functor ((<&>))
class IsNetworkCtx v where
  toNetworkContext :: v-> IO NetworkContext
  toFullNetworkContext :: v->IO FullNetworkContext
  getConn :: v-> LocalNodeConnectInfo CardanoMode
  networkCtxNetwork :: v -> NetworkId

data MinimalNetworkContext=
    MinimalNetworkContext {
      conn :: LocalNodeConnectInfo CardanoMode,
      network :: NetworkId
    }


instance IsNetworkCtx MinimalNetworkContext where
  toNetworkContext  (MinimalNetworkContext conn network)=
        queryProtocolParam conn <&> NetworkContext conn network
  toFullNetworkContext  (MinimalNetworkContext conn network) = do
        eHistory <-queryEraHistory conn
        systemStart <-querySystemStart conn
        protocolPrams <-queryProtocolParam conn
        pure  $ FullNetworkContext conn network protocolPrams  systemStart eHistory
  getConn (MinimalNetworkContext conn _)= conn 
  networkCtxNetwork (MinimalNetworkContext conn network)=network 

data  NetworkContext=NetworkContext {
      ctxConn :: LocalNodeConnectInfo CardanoMode,
      ctxNetwork :: NetworkId,
      ctxProtocolParameters:: ProtocolParameters
    }
instance IsNetworkCtx NetworkContext where
  toNetworkContext  = pure
  toFullNetworkContext  (NetworkContext conn network pParams) = do
        eHistory <-queryEraHistory conn
        systemStart <-querySystemStart conn
        pure  $ FullNetworkContext conn network pParams  systemStart eHistory
  getConn (NetworkContext conn _ _)= conn 
  networkCtxNetwork (NetworkContext _ net _ ) = net

data  FullNetworkContext=FullNetworkContext  {
      fctxConn :: LocalNodeConnectInfo CardanoMode,
      fctxNetwork :: NetworkId,
      fctxProtocolParameters:: ProtocolParameters,
      fctxSystemStart :: SystemStart,
      fctxEraHistory :: EraHistory CardanoMode
    }

instance IsNetworkCtx FullNetworkContext where
  toNetworkContext  (FullNetworkContext conn network pParam _ _ ) = pure $ NetworkContext conn network pParam
  toFullNetworkContext =pure
  getConn (FullNetworkContext conn _ _ _ _)= conn 
  networkCtxNetwork (FullNetworkContext _ network _ _ _ )=network


getDefaultMainnetContext :: IO MinimalNetworkContext
getDefaultMainnetContext =  do 
  conn <-getDefaultConnection 
  pure $ MinimalNetworkContext conn Mainnet

getDefaultTestnetContext :: IO MinimalNetworkContext
getDefaultTestnetContext = do
  conn <-getDefaultConnection 
  pure $ MinimalNetworkContext conn (Testnet  (NetworkMagic 1097911063))
