{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Test.ChainApiTests where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Parsers (parseAddress, parseAddressBech32, parseTxIn)
import Cardano.Kuber.Util (addressInEraToAddressAny)
import Cardano.Slotting.Time
import Data.Functor.Identity (Identity)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace as Debug
import Test.TestGen

test_kGetNetworkId :: (HasChainQueryAPI api) => Kontract api w FrameworkError NetworkId
test_kGetNetworkId = kGetNetworkId

test_kQueryProtocolParams :: (HasChainQueryAPI api) => Kontract api w FrameworkError (LedgerProtocolParameters BabbageEra)
test_kQueryProtocolParams = kQueryProtocolParams

test_kQuerySystemStart :: (HasChainQueryAPI api) => Kontract api w FrameworkError SystemStart
test_kQuerySystemStart = kQuerySystemStart

test_kQueryGenesisParams :: (HasChainQueryAPI api) => Kontract api w FrameworkError (GenesisParameters ShelleyEra)
test_kQueryGenesisParams = kQueryGenesisParams

test_kQueryUtxoByAddress :: (HasChainQueryAPI api) => Kontract api w FrameworkError (UTxO BabbageEra)
test_kQueryUtxoByAddress = kQueryUtxoByAddress $ Set.singleton $ addressInEraToAddressAny dummyAddressInBabbageEra

test_kQueryUtxoByTxin :: (HasChainQueryAPI api) => Kontract api w FrameworkError (UTxO BabbageEra)
test_kQueryUtxoByTxin = kQueryUtxoByTxin $ Set.singleton dummyTxIn

test_kQueryChainPoint :: (HasChainQueryAPI api) => Kontract api w FrameworkError ChainPoint
test_kQueryChainPoint = kQueryChainPoint

test_kQueryCurrentEra :: (HasChainQueryAPI api) => Kontract api w FrameworkError AnyCardanoEra
test_kQueryCurrentEra = kQueryCurrentEra
