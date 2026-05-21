import Cardano.Kuber.Api
import Control.Exception (finally)
import Control.Exception (throw)
import Control.Monad.IO.Class
import Debug.Trace as Debug
import System.Directory (getCurrentDirectory)
import Test.ApiTest
import Test.ChainApiTests
import Test.IntegrationSetup (bootstrapIntegration)
import Test.KuberApiTests
import qualified Test.ParserTest as ParserTest
import Test.Tasty

main :: IO ()
main = do
  cleanup <- bootstrapIntegration
  defaultMain tests `finally` cleanup

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
      testQueryCurrentEra
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
