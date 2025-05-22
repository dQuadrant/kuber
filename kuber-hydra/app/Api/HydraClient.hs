{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.HydraClient where

import Cardano.Api
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models (AddressModal)
import Data.Set
import Websocket.Aeson
import Websocket.TxBuilder
import Websocket.Utils

newtype HydraConfig = HydraConfig AppConfig

instance HasChainQueryAPI HydraConfig where
  kQueryUtxoByAddress :: (IsTxBuilderEra era) => Set AddressAny -> Kontract HydraConfig w FrameworkError (UTxO era)
  kQueryUtxoByAddress addrs = KLift $ \(HydraConfig c) -> do
    queryUTxO c (setOfAddressAnyToAddressesInConwayEra addrs) []

-- pure $ Left  (FrameworkError LibraryError "Reeshav is done working on it (mentally)")
