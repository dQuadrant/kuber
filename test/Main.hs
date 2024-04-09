import Cardano.Kuber.Api
import Control.Exception (throw)
import Control.Monad.IO.Class
import Debug.Trace as Debug
import System.Directory (getCurrentDirectory)
import Test.ApiTest
import Test.ChainApiTests
import Test.KuberApiTests
import qualified Test.ParserTest as ParserTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [chainApiTests, kuberApiTests]

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
      testBuildTxSupportMetadata,
      testBuildTxSupportDatumInAuxData,
      testExUnits
    ]
