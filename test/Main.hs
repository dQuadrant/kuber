import qualified Test.AddressParsing as AddressParsing
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "kuber" [
         AddressParsing.tests
    ]