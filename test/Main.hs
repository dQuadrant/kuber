import qualified Test.ParserTest as ParserTest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "kuber" [
         ParserTest.tests
    ]