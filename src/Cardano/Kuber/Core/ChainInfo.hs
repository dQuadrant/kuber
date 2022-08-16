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
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Core as Ledger


-- ChainInfo class represents a class of objects that wraps cardano node connection information 
-- and a bunch of other information cached with it that might be needed later
class ChainInfo v where
  withProtocolParam :: v-> IO ChainInfoWithProtocolParams
  withDetails :: v->IO DetailedChainInfo
  getConnectInfo :: v-> LocalNodeConnectInfo CardanoMode

  getNetworkId :: v -> NetworkId
  getNetworkId v = localNodeNetworkId $ getConnectInfo v 

-- ChainConnectInfo wraps (LocalNodeConnectInfo CardanoMode)
-- This is the minimal information required to connect to a cardano node
newtype ChainConnectInfo= ChainConnectInfo (LocalNodeConnectInfo CardanoMode)
 

instance ChainInfo ChainConnectInfo where
  withProtocolParam  (ChainConnectInfo conn )=
        queryProtocolParam conn <&> ChainInfoWithProtocolParams conn 
  withDetails  (ChainConnectInfo conn ) = do
        eHistory <-queryEraHistory conn
        systemStart <-querySystemStart conn
        protocolPrams <-queryProtocolParam conn
        print $ "CardanoNode " ++ show systemStart 
        Lovelace costPerWord <-case protocolParamUTxOCostPerWord protocolPrams of
          Nothing -> fail "Missing Cost per bytes of Transaction in protocol Parameters"
          Just lo -> pure lo
        let ledgerPparam = toLedgerPParams  ShelleyBasedEraAlonzo  protocolPrams
        pure  $ DetailedChainInfo costPerWord conn  protocolPrams  ledgerPparam  systemStart eHistory
  getConnectInfo (ChainConnectInfo conn )= conn 
  getNetworkId (ChainConnectInfo conn )=localNodeNetworkId conn 

-- Along with the NodeConnectInfo, it caches Protocol parameters to be used during transaction creation
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
          Nothing -> fail "Missing Cost per bytes of transaction in protocol paremeters"
          Just lo -> pure lo
        let ledgerPparam = toLedgerPParams  ShelleyBasedEraAlonzo  pParams
        pure  $ DetailedChainInfo costPerWord conn   pParams (ledgerPparam)  systemStart eHistory
  getConnectInfo (ChainInfoWithProtocolParams conn  _)= conn 


-- Along with the NodeconnectInfo, it caches protocolParameters, CostPerBytes in transaction, systemstart and eraHistory.
-- These information are cached to be used during transaction construction/balancing and execution unitcalculation
data  DetailedChainInfo=DetailedChainInfo  {
      dciCostPerWord ::  Integer,
      dciConn :: LocalNodeConnectInfo CardanoMode,
      dciProtocolParams:: ProtocolParameters,
      dciPparm ::Ledger.PParams (ShelleyLedgerEra AlonzoEra) ,
      dciSystemStart :: SystemStart,
      dciEraHistory :: EraHistory CardanoMode
    } 


instance ChainInfo DetailedChainInfo where
  withProtocolParam  (DetailedChainInfo  fctxcpw conn  pParam _ _ _ ) = pure $ ChainInfoWithProtocolParams conn  pParam
  withDetails =pure
  getConnectInfo (DetailedChainInfo fctxcpw conn _ _  _ _)= conn 
