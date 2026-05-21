module Cardano.Kuber.Core.ChainInfo
where

import Cardano.Api


-- class ChainInfo v where
--   getConnectInfo :: v-> LocalNodeConnectInfo CardanoMode
--   getNetworkId :: v -> NetworkId


-- ChainConnectInfo wraps (LocalNodeConnectInfo CardanoMode)
-- This is the minimal information required to connect to a cardano node
-- newtype ChainConnectInfo= ChainConnectInfo (LocalNodeConnectInfo CardanoMode)


data KuberConnectInfo = KuberConnectInfo {
    kuberUrl  ::  String
  , apiKey    ::  Maybe String
  , networkId :: NetworkId
}

