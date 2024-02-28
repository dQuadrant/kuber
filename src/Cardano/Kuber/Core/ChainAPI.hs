{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Kuber.Core.ChainAPI where
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Error
import Cardano.Api.Shelley
import Cardano.Api
import Cardano.Slotting.Time (SystemStart)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Set (Set)
import PlutusTx.Prelude (traceError)
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Api.Ledger (StandardCrypto, GovState, DRepState, Credential, KeyRole (DRepRole))
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra)
import Data.Map (Map)


class HasChainQueryAPI a  where
  -- Core query functions
  kGetNetworkId         :: Kontract  a w FrameworkError NetworkId
  kQueryProtocolParams  :: IsTxBuilderEra era => Kontract  a w FrameworkError (LedgerProtocolParameters era)
  kQuerySystemStart     :: Kontract a w FrameworkError  SystemStart
  kQueryGenesisParams   :: Kontract a w FrameworkError (GenesisParameters ShelleyEra)
  kQueryUtxoByAddress   :: IsTxBuilderEra era => Set AddressAny -> Kontract  a w FrameworkError (UTxO era)
  kQueryUtxoByTxin      :: IsTxBuilderEra era => Set TxIn -> Kontract a w FrameworkError (UTxO era)
  kQueryChainPoint      :: Kontract a w FrameworkError ChainPoint
  kQueryCurrentEra      :: Kontract a w FrameworkError AnyCardanoEra
  kQueryStakeDeposit    :: Set StakeCredential -> Kontract a w FrameworkError (Map StakeCredential Lovelace)
  kQueryDrepState       :: Set (Credential 'DRepRole StandardCrypto) -> Kontract a w FrameworkError (Map (Credential 'DRepRole StandardCrypto) (DRepState StandardCrypto))
  kQueryGovState        :: IsTxBuilderEra era => Kontract a w FrameworkError (GovState (ShelleyLedgerEra era))

class HasSubmitApi a where
  kSubmitTx :: InAnyCardanoEra Tx ->  Kontract  a w FrameworkError ()

data  CachedApi  a = CachedApi a  SystemStart ProtocolParameters EraHistory (GenesisParameters ShelleyEra)


-- instance HasChainQueryAPI a => HasChainQueryAPI (CachedApi a ) where
--   kQueryProtocolParams = KLift $ \(CachedApi a _ pp _  _ )  -> pure (pure pp)
--   kQuerySystemStart = KLift $ \(CachedApi a ss pp eh  gp )  -> pure (pure ss)
--   kQueryEraHistory = KLift $ \(CachedApi a ss pp eh  gp )  -> pure (pure eh)
--   kQueryGenesisParams = KLift $ \(CachedApi a ss pp eh  gp )  -> pure (pure gp)
--   kQueryUtxoByAddress addrs=  KLift $ \(CachedApi a _ _ _  _ )  -> evaluateKontract a (kQueryUtxoByAddress addrs)
--   kQueryUtxoByTxin txins = KLift $ \(CachedApi a _ _ _  _ )  -> evaluateKontract a (kQueryUtxoByTxin txins)
--   kQueryChainPoint = KLift $ \(CachedApi a _ _ _  _ )  -> evaluateKontract a kQueryChainPoint
--   kGetNetworkId = KLift $ \(CachedApi a _ _ _  _ )  ->evaluateKontract a kGetNetworkId


-- instance HasSubmitApi a => HasSubmitApi( CachedApi a) where
--     kSubmitTx  tx =  KLift $ \(CachedApi a _ pp _  _ )  ->evaluateKontract a (kSubmitTx tx)



-- withCache :: HasChainQueryAPI a => a -> IO (Either FrameworkError (CachedApi a))
-- withCache api = do
--     eParams  <- evaluateKontract api $  do
--                     pParam <-  kQueryProtocolParams
--                     sStart <- kQuerySystemStart
--                     eraHistory  <- kQueryEraHistory
--                     genesisParams <- kQueryGenesisParams
--                     pure ( pParam, sStart,eraHistory , genesisParams)
--     case eParams of
--       Left fe -> pure $ Left fe
--       Right (pp, ss, eh, gp) -> pure $ pure $ CachedApi api ss pp eh gp
