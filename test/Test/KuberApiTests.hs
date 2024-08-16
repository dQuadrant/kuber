{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.KuberApiTests where

import Cardano.Api
import Cardano.Kuber.Api
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Debug.Trace as Debug
import GHC.IO
import Hedgehog.Gen
import PlutusTx.Trace
import System.Directory
import Test.TestGen

txMaker :: FilePath -> TxBuilder_ BabbageEra
txMaker txJsonFileName =
  case A.decode txJson of
    Nothing -> error "Invalid Tx"
    Just any -> any :: TxBuilder_ BabbageEra
  where
    filePath :: FilePath
    filePath = unsafePerformIO getCurrentDirectory ++ "/test/Test/TransactionJSON/" ++ txJsonFileName ++ ".json"

    txJson :: BS8.ByteString
    txJson = unsafePerformIO $ BS8.readFile filePath

test_kBuildTx_simplePay :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_simplePay = kBuildTx $ txMaker "simplePay"

test_kBuildTx_simpleMint :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_simpleMint = kBuildTx $ txMaker "simpleMint"

test_kBuildTx_redeemWithReferenceInput :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_redeemWithReferenceInput = kBuildTx $ txMaker "redeemWithReferenceInput"

test_kBuildTx_redeemFromSmartContract :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_redeemFromSmartContract = kBuildTx $ txMaker "redeemFromSmartContract"

test_kBuildTx_supportMetadata :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_supportMetadata = kBuildTx $ txMaker "simplePayWithMetadata"

test_kBuildTx_supportDatumInAuxData :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_kBuildTx_supportDatumInAuxData = kBuildTx $ txMaker "mintWithDatumInAuxData"

test_ex_units :: (HasKuberAPI api, IsTxBuilderEra BabbageEra) => Kontract api w FrameworkError (Tx BabbageEra)
test_ex_units = kBuildTx $ txMaker "exUnitsMultisig"