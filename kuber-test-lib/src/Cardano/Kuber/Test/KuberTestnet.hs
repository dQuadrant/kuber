{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Cardano.Kuber.Test.KuberTestnet
where
import Cardano.Api
import Cardano.Kuber.Api
import Cardano.Kuber.Test.HasTestnet
import Cardano.Api.Shelley
import Data.Maybe (fromJust)
import System.FilePath ((</>))
import Control.Exception (Exception)
import Hedgehog (MonadTest, Test)
import Hedgehog.Internal.Property (liftTest, runTest)


data KuberTestnet = KuberTestnet {
        ktConnInfo :: LocalNodeConnectInfo
    ,   ktGenesisWallet :: [EnterpriseWallet]
    ,   ktSpos :: [TestnetSpo]
} 



instance HasTestnet KuberTestnet where
  testNetPools :: Kontract KuberTestnet w FrameworkError [TestnetSpo]
  testNetPools = KLift $ \(KuberTestnet c ws spo)  -> pure $ pure spo

  testNetWallets :: Kontract KuberTestnet w FrameworkError [EnterpriseWallet]
  testNetWallets = KLift $ \(KuberTestnet c ws spo)  -> pure $ pure ws

instance HasChainQueryAPI KuberTestnet where
    kQueryProtocolParams :: IsTxBuilderEra era => Kontract KuberTestnet w FrameworkError (LedgerProtocolParameters era)
    kQueryProtocolParams = withConnectInfo kQueryProtocolParams
    kQuerySystemStart = withConnectInfo kQuerySystemStart
    kQueryGenesisParams = withConnectInfo kQueryGenesisParams
    kQueryUtxoByAddress = withConnectInfo2 kQueryUtxoByAddress
    kQueryUtxoByTxin = withConnectInfo2 kQueryUtxoByTxin
    kQueryChainPoint  = withConnectInfo kQueryChainPoint
    kGetNetworkId =  withConnectInfo kGetNetworkId
    kQueryCurrentEra = withConnectInfo kQueryCurrentEra
    kQueryGovState = withConnectInfo kQueryGovState
    kQueryStakeDeposit = withConnectInfo2 kQueryStakeDeposit
    kQueryDrepState  = withConnectInfo2 kQueryDrepState
    kQueryDRepDistribution = withConnectInfo2 kQueryDRepDistribution

instance HasKuberAPI KuberTestnet where
    kTxBuildTxBody = withConnectInfo2 kTxBuildTxBody
    kBuildTx = withConnectInfo2 kBuildTx
    kTimeToSlot = withConnectInfo2 kTimeToSlot
    kSlotToTime = withConnectInfo2 kSlotToTime
    kEvaluateExUnits = withConnectInfo2 kEvaluateExUnits
    kCalculateMinFee = withConnectInfo2 kCalculateMinFee
    kBuildAndSubmit = withConnectInfo2 kBuildAndSubmit

instance HasSubmitApi KuberTestnet where
  kSubmitTx :: InAnyCardanoEra Tx -> Kontract KuberTestnet w FrameworkError ()
  kSubmitTx = withConnectInfo2 kSubmitTx

instance HasLocalNodeAPI  KuberTestnet where
  kQueryEraHistory :: Kontract KuberTestnet w FrameworkError EraHistory
  kQueryEraHistory = withConnectInfo kQueryEraHistory
    

withConnectInfo2 ::  (a ->Kontract LocalNodeConnectInfo w FrameworkError r ) -> a -> Kontract KuberTestnet w  FrameworkError r
withConnectInfo2   f a = withConnectInfo (f a) 

withConnectInfo ::Kontract LocalNodeConnectInfo w FrameworkError r -> Kontract KuberTestnet w  FrameworkError r
withConnectInfo q = kUpdateApi  q ktConnInfo


kUpdateApi ::  Kontract a w FrameworkError r -> (b -> a) -> Kontract b w FrameworkError r
kUpdateApi q f = KLift $ \(a1 )  ->  
        evaluateKontract (f a1) (q)
            
instance MonadTest  (Kontract KuberTestnet w FrameworkError) where
  liftTest :: Test a -> Kontract KuberTestnet w FrameworkError a
  liftTest test =  KLift $ \_ -> do
            let (result,journal) = runTest test
            case result of 
                Left e -> pure $ Left $ FrameworkError LibraryError (show e) 
                Right v -> pure $ pure v
    


{- | Signing key and address for wallet 1
  Handles two key types: GenesisUTxOKey and PaymentKey
-}
wAll ::
    (MonadIO m) =>
    FilePath ->
    Integer -> 
    NetworkId ->
    m EnterpriseWallet
wAll tempAbsPath walletId networkId = do
    let w1VKeyFile = File $ tempAbsPath </> "utxo-keys/utxo" <> show walletId <> "/utxo.vkey"
        w1SKeyFile = File $ tempAbsPath </> "utxo-keys/utxo" <> show walletId <> "/utxo.skey"


    -- GenesisUTxOKey comes from cardano-testnet
    let genesisVKey vkeyFile = maybeReadAs (AsVerificationKey AsGenesisUTxOKey) vkeyFile
        genesisSKey skeyFile = maybeReadAs (AsSigningKey AsGenesisUTxOKey) skeyFile
        paymentVkey vkeyFile = maybeReadAs (AsVerificationKey AsPaymentKey) vkeyFile
        paymentSkey skeyFile = maybeReadAs (AsSigningKey AsPaymentKey) skeyFile
    mGenesisVKey1 :: Maybe (VerificationKey GenesisUTxOKey) <- genesisVKey w1VKeyFile
    mGenesisSKey1 :: Maybe (SigningKey GenesisUTxOKey) <- genesisSKey w1SKeyFile
    mPaymentVKey1 :: Maybe (VerificationKey PaymentKey) <- paymentVkey w1VKeyFile
    mPaymentSKey1 :: Maybe (SigningKey PaymentKey) <- paymentSkey w1SKeyFile
    let

        vKey1 :: VerificationKey PaymentKey = maybe (fromJust mPaymentVKey1) castVerificationKey mGenesisVKey1
        sKey1 :: SigningKey PaymentKey = maybe (fromJust mPaymentSKey1) castSigningKey mGenesisSKey1
        address =   makeShelleyAddress
                        networkId
                        (PaymentCredentialByKey $ verificationKeyHash vKey1)
                        NoStakeAddress

    pure $ EnterpriseWallet sKey1 address


pool1All ::
    (MonadIO m) =>
    FilePath ->
    m TestnetSpo
pool1All tempAbsPath = do
    let pool1SKeyFile = File $ tempAbsPath </> "pools/cold1.skey"
    mPool1SKey :: Maybe (SigningKey StakePoolKey) <-
        maybeReadAs (AsSigningKey AsStakePoolKey) pool1SKeyFile
    let pool1SKey = fromJust mPool1SKey

    let pool1VerificationKeyFile = File $ tempAbsPath </> "pools/cold1.vkey"
    mPool1VKey :: Maybe (VerificationKey StakePoolKey) <-
        maybeReadAs (AsVerificationKey AsStakePoolKey) pool1VerificationKeyFile
    let pool1VKey = fromJust mPool1VKey
        pool1VKeyHash = verificationKeyHash pool1VKey
        StakePoolKeyHash pool1StakePoolKeyHash = pool1VKeyHash

    let pool1StakingRewardsFile = File $ tempAbsPath </> "pools/staking-reward1.vkey"
    mPool1StakingRewards :: Maybe (VerificationKey StakeKey) <-
        maybeReadAs (AsVerificationKey AsStakeKey) pool1StakingRewardsFile
    let pool1StakeKeyHash = verificationKeyHash (fromJust mPool1StakingRewards)

    let pool1VrfKeyFile = File $ tempAbsPath </> "pools/vrf1.vkey"
    mPool1VrfKey :: Maybe (VerificationKey VrfKey) <-
        maybeReadAs (AsVerificationKey AsVrfKey) pool1VrfKeyFile
    let pool1VrfKeyHash = verificationKeyHash (fromJust mPool1VrfKey)

    return $
        TestnetSpo
            pool1SKey
            pool1VKey
            pool1VKeyHash
            pool1StakeKeyHash
            pool1VrfKeyHash
            pool1StakePoolKeyHash


maybeReadAs ::
    (HasTextEnvelope a, MonadIO m) => AsType a -> File content 'In -> m (Maybe a)
maybeReadAs as file@(File fp) = do
    maybeEither . liftIO $ readFileTextEnvelope as file

maybeEither m = m >>= return . either (const Nothing) Just