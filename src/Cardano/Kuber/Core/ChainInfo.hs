module Cardano.Kuber.Core.ChainInfo
where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Slotting.Time
import Debug.Trace as Debug
import Data.Functor ((<&>))
import Cardano.Kuber.Utility.QueryHelper
import Data.ByteString.Char8 (unpack)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Core as Ledger
import Data.Set (Set)
import Data.Time.Clock.POSIX (POSIXTime)


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


