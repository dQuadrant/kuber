import Cardano.Kuber.Api
import Control.Exception (finally)
import Control.Exception (throw)
import Control.Monad.IO.Class
import Debug.Trace as Debug
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import Test.ApiTest
import Test.ChainApiTests
import Test.IntegrationSetup (bootstrapIntegration)
import Test.KuberApiTests
import qualified Test.ParserTest as ParserTest
import Test.Tasty
import Test.Tasty.Runners (NumThreads (NumThreads))
import Text.Read (readMaybe)

main :: IO ()
main = do
  cleanup <- bootstrapIntegration
  numThreads <- getTestThreads
  defaultMain (localOption (NumThreads numThreads) tests) `finally` cleanup

getTestThreads :: IO Int
getTestThreads = do
  value <- lookupEnv "KUBER_TEST_NUM_THREADS"
  pure $ case value >>= readMaybe of
    Just n | n > 0 -> n
    _ -> 1

tests :: TestTree
tests = testGroup "Tests" [chainApiTests, kuberApiTests, ParserTest.tests]

chainApiTests :: TestTree
chainApiTests =
  testGroup
    "ChainAPI"
    [ testGetNetworkId,
      testQueryProtocolParams,
      testQuerySystemStart,
      testQueryGenesisParams,
      testQueryUtxoByAddress,
      testQueryUtxoByTxin,
      testQueryChainPoint,
      testQueryCurrentEra,
      testQueryEraHistory
    ]

kuberApiTests :: TestTree
kuberApiTests =
  testGroup
    "KuberAPI"
    [ testBuildTxSimplePay,
      testBuildTxSimpleMint,
      testBuildTxSimpleRedeem,
      testBuildTxRedeemFromSmartContract,
      testRedeemFromSmartContractE2E,
      testBuildTxSupportMetadata,
      testBuildTxSupportDatumInAuxData,
      testExUnits,
      testCalculateFee,
      testSubmitTx
    ]
