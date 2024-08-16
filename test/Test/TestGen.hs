{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Test.TestGen where

import Cardano.Api
import Cardano.Api.Shelley
import Data.Functor
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import Hedgehog (Gen)
import Hedgehog.Gen (choice, element)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen (GenT)
import qualified Hedgehog.Range as Range
import Test.Gen.Cardano.Api.Typed
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Parsers
import Cardano.Api.Ledger (Coin(..))

genSomeWalelt :: NetworkId -> Gen TxBuilder
genSomeWalelt netid = do
  vkeys <- Gen.list (Range.linear 2 5) (genVerificationKey AsPaymentKey)
  userStake <- genStakeAddressReference

  let addresses = map (\vkey -> makeShelleyAddressInEra ShelleyBasedEraConway netid (PaymentCredentialByKey $ verificationKeyHash vkey) userStake) vkeys
      genutxo genVal = do
        value <- genVal
        txid <- genTxIn
        address <- element addresses
        pure (txid, TxOut address (TxOutValueShelleyBased ShelleyBasedEraConway value) TxOutDatumNone ReferenceScriptNone)
      genAdaUtxo genVal = do
        value <- genVal
        txid <- genTxIn
        address <- element addresses
        pure (txid, TxOut address (TxOutValueByron value) TxOutDatumNone ReferenceScriptNone)
      genAdaVal = do
        Gen.integral (Range.linear 2_000_000 3_000_000_000_000) <&> Coin
      genCollateralVal = do
        Gen.integral (Range.linear 5_000_000 10_000_000) <&> Coin
  utxos <- Gen.list (Range.linear 4 10) (genutxo (genValueForTxOut ShelleyBasedEraConway))
  adaUtxos <- Gen.list (Range.linear 4 10) (genAdaUtxo genAdaVal)
  collateralUtxo <- genAdaUtxo genCollateralVal
  pure $ txWalletUtxos (UTxO . Map.fromList $ collateralUtxo : utxos <> adaUtxos)

chainApiNetworkIdTest :: (HasChainQueryAPI api) => Kontract api w FrameworkError NetworkId
chainApiNetworkIdTest = kGetNetworkId

genPubkeyAddressShelley :: Gen (Address ShelleyAddr)
genPubkeyAddressShelley = do
  vKeyPayment <- genVerificationKey AsPaymentKey
  makeShelleyAddress
    Mainnet
    (PaymentCredentialByKey $ verificationKeyHash vKeyPayment)
    <$> genStakeAddressReference

genAddressSet :: GenT Identity (Set AddressAny)
genAddressSet = do
  shellyAddress <- genPubkeyAddressShelley
  let sets = Set.fromList [toAddressAny shellyAddress]
  return sets

dummyAddressInBabbageEra :: AddressInEra BabbageEra
dummyAddressInBabbageEra = case parseAddress (T.pack "addr_test1qpw57k84mzwmpqyx6n9smye79mxt8rqpfpvx7p6chz95sm7a3aw5tgv4fc9weuwme2u29aerv5hk0m2lkpkgasn7gtxqwen0r7") of
  Just a -> a

dummyAddressInConwayEra = do
  shellyAddr <- genPubkeyAddressShelley
  let res = anyAddressInEra ConwayEra (toAddressAny shellyAddr)
  case res of
    Right aie -> pure aie

dummyTxIn :: TxIn
dummyTxIn = case parseTxIn (T.pack "2bc637151310fa30a08d2cf8756b9bf7cef4e378243ee2018a15fd5811d314c7#1") of
  Just a -> a