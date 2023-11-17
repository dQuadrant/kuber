{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.KuberApiTests where

import Cardano.Api
import Cardano.Kuber.Api
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Debug.Trace as Debug
import Hedgehog.Gen
import PlutusTx.Trace
import Test.TestGen
import Test.TxBuilderJSON

txMaker :: [Char] -> TxBuilder_ BabbageEra
txMaker txString = case A.decode $ BS8.pack txString of
  Nothing -> traceError "Invalid Tx"
  Just any -> any :: TxBuilder_ BabbageEra

test_kBuildTx_simplePay :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_simplePay = kBuildTx $ txMaker simplePay

test_kBuildTx_simpleMint :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_simpleMint = kBuildTx $ txMaker simpleMint

test_kBuildTx_redeemWithReferenceInput :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_redeemWithReferenceInput = kBuildTx $ txMaker redeemWithReferenceInput

test_kBuildTx_redeemFromSmartContract :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_redeemFromSmartContract = kBuildTx $ txMaker redeemFromSmartContract

test_kBuildTx_supportMetadata :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_supportMetadata = kBuildTx $ txMaker simplePayWithMetadata


