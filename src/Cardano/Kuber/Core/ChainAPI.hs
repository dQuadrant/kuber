{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Cardano.Api.Ledger (StandardCrypto, GovState, DRepState, Credential, KeyRole (DRepRole), DRep, Coin)
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra)
import Data.Map (Map)


class HasChainQueryAPI a  where
  -- Core query functions
  kQueryProtocolParams    :: IsTxBuilderEra era => Kontract  a w FrameworkError (LedgerProtocolParameters era)
  kQueryUtxoByAddress     :: IsTxBuilderEra era => Set AddressAny -> Kontract  a w FrameworkError (UTxO era)
  kQueryUtxoByTxin        :: IsTxBuilderEra era => Set TxIn -> Kontract a w FrameworkError (UTxO era)
  kQueryChainPoint        :: Kontract a w FrameworkError ChainPoint

class HasCardanoQueryApi a where
  kQuerySystemStart       :: Kontract a w FrameworkError  SystemStart  -- for hydra this one is init timestamp.
  kGetNetworkId           :: Kontract  a w FrameworkError NetworkId
  kQueryGenesisParams     :: Kontract a w FrameworkError (GenesisParameters ShelleyEra)
  kQueryCurrentEra        :: Kontract a w FrameworkError AnyCardanoEra
  kQueryStakeDeposit      :: Set StakeCredential -> Kontract a w FrameworkError (Map StakeCredential Coin)
  kQueryDrepState         :: Set (Credential 'DRepRole StandardCrypto) -> Kontract a w FrameworkError (Map (Credential 'DRepRole StandardCrypto) (DRepState StandardCrypto))
  kQueryGovState          :: IsTxBuilderEra era => Kontract a w FrameworkError (GovState (ShelleyLedgerEra era))
  kQueryDRepDistribution  :: Set (DRep StandardCrypto) -> Kontract a w FrameworkError (Map   (DRep StandardCrypto)  Coin)

  

class HasSubmitApi a where
  kSubmitTx :: InAnyCardanoEra Tx ->  Kontract  a w FrameworkError ()