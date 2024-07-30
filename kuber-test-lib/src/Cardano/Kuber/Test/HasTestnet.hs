{-# LANGUAGE DataKinds #-}
module Cardano.Kuber.Test.HasTestnet 
where
import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Api.Ledger hiding (Tx)
import Cardano.Kuber.Api

data ShelleyWallet = ShelleyWallet {
      wPaymentSkey :: SigningKey PaymentKey
    , wStakeSkey :: SigningKey StakeKey
    , wAddress :: Address ShelleyAddr
} deriving (Show)

data EnterpriseWallet = EnterpriseWallet {
      ewPaymentSkey :: SigningKey PaymentKey
    , ewAddress :: Address ShelleyAddr
} deriving (Show)


data TestnetSpo = TestnetSpo
    { 
      spoSKey :: SigningKey StakePoolKey
    , spoVKey :: VerificationKey StakePoolKey
    , spoVKeyHash :: Hash StakePoolKey
    , spoKeyHash :: Hash StakeKey
    , spoVrfHash :: Hash VrfKey
    , spoPoolKeyHash :: KeyHash 'StakePool StandardCrypto
    }
    deriving (Show)


class HasTestnet a where
  testNetPools :: Kontract a w FrameworkError [TestnetSpo]
  testNetWallets  :: Kontract a w  FrameworkError [EnterpriseWallet]
