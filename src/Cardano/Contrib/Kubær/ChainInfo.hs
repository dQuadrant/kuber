module Cardano.Contrib.Kubær.ChainInfo
where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Slotting.Time
import Cardano.Contrib.Kubær.Util
    ( getDefaultConnection,
      queryProtocolParam,
      querySystemStart,
      queryEraHistory, getNetworkFromEnv )
import Data.Functor ((<&>))

class ChainInfo v where
  withProtocolParam :: v-> IO ChainInfoWithProtocolParams
  withDetails :: v->IO DetailedChainInfo
  getConnectInfo :: v-> LocalNodeConnectInfo CardanoMode

  getNetworkId :: v -> NetworkId
  getNetworkId v = localNodeNetworkId $ getConnectInfo v 

newtype ChainConnectInfo= ChainConnectInfo (LocalNodeConnectInfo CardanoMode)
 
instance ChainInfo ChainConnectInfo where
  withProtocolParam  (ChainConnectInfo conn )=
        queryProtocolParam conn <&> ChainInfoWithProtocolParams conn 
  withDetails  (ChainConnectInfo conn ) = do
        eHistory <-queryEraHistory conn
        systemStart <-querySystemStart conn
        protocolPrams <-queryProtocolParam conn
        Lovelace costPerWord <-case protocolParamUTxOCostPerWord protocolPrams of
          Nothing -> fail "error here"
          Just lo -> pure lo
        pure  $ DetailedChainInfo costPerWord conn  protocolPrams  systemStart eHistory
  getConnectInfo (ChainConnectInfo conn )= conn 
  getNetworkId (ChainConnectInfo conn )=localNodeNetworkId conn 

data  ChainInfoWithProtocolParams=ChainInfoWithProtocolParams {
      ctxConn :: LocalNodeConnectInfo CardanoMode,
      ctxProtocolParameters:: ProtocolParameters
    }
instance ChainInfo ChainInfoWithProtocolParams where
  withProtocolParam  = pure
  withDetails  (ChainInfoWithProtocolParams conn  pParams) = do
        eHistory <-queryEraHistory conn
        systemStart <-querySystemStart conn
        Lovelace costPerWord <-case protocolParamUTxOCostPerWord pParams of
          Nothing -> fail "error here"
          Just lo -> pure lo
        pure  $ DetailedChainInfo costPerWord conn   pParams  systemStart eHistory
  getConnectInfo (ChainInfoWithProtocolParams conn  _)= conn 

data  DetailedChainInfo=DetailedChainInfo  {
      dciCostPerWord ::  Integer,
      dciConn :: LocalNodeConnectInfo CardanoMode,
      dciProtocolParams:: ProtocolParameters,
      dciSystemStart :: SystemStart,
      dciEraHistory :: EraHistory CardanoMode
    }

instance ChainInfo DetailedChainInfo where
  withProtocolParam  (DetailedChainInfo  fctxcpw conn  pParam _ _ ) = pure $ ChainInfoWithProtocolParams conn  pParam
  withDetails =pure
  getConnectInfo (DetailedChainInfo fctxcpw conn _  _ _)= conn 


getDefaultMainnetContext :: IO ChainConnectInfo
getDefaultMainnetContext =  do 
  conn <-getDefaultConnection "mainnet" Mainnet
  pure $ ChainConnectInfo conn 

getDefaultTestnetContext :: IO ChainConnectInfo
getDefaultTestnetContext = do
  let network=Testnet  (NetworkMagic 1097911063)
  conn <-getDefaultConnection  "testnet" network
  pure $ ChainConnectInfo conn 

readContextFromEnv :: IO ChainConnectInfo
readContextFromEnv = readContextFromEnv' "NETWORK"

readContextFromEnv' :: String -> IO ChainConnectInfo
readContextFromEnv' envKey = do 
  v <- getNetworkFromEnv envKey
  case v of 
    Mainnet -> getDefaultMainnetContext
    (Testnet  (NetworkMagic 1097911063)) -> getDefaultTestnetContext
    net ->  do 
      conn <- getDefaultConnection "" net
      pure $ ChainConnectInfo conn  
