{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Cardano.Kuber.Core.ChainAPI where
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Error
import Cardano.Api.Shelley
import Data.Set (Set)
import Cardano.Ledger.Keys (DRepRole)
import Cardano.Api.Ledger (GovState, DRepState, Credential, DRep, Coin)
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
  kQueryDrepState         :: Set (Credential DRepRole) -> Kontract a w FrameworkError (Map (Credential DRepRole) DRepState)
  kQueryGovState          :: IsTxBuilderEra era => Kontract a w FrameworkError (GovState (ShelleyLedgerEra era))
  kQueryDRepDistribution  :: Set DRep -> Kontract a w FrameworkError (Map DRep Coin)

  

class HasSubmitApi a where
  kSubmitTx :: InAnyCardanoEra Tx ->  Kontract  a w FrameworkError ()