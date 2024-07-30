{-# LANGUAGE OverloadedStrings #-}
import Cardano.Kuber.Test.Testnet (runTestnet)
import Hedgehog.Main (defaultMain)
import Hedgehog 
import Cardano.Kuber.Api
import Cardano.Kuber.Test.KuberTestnet
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Test.Tasty (testGroup)
import Test.Tasty.Providers
import Cardano.Api



main= runTestnet

-- Example main function
-- main' :: IO ()
-- main' = do
--   (ChainConnectInfo connInfo) <- chainInfoFromEnv
--   let 
--       kuberTesnet = KuberTestnet connInfo [] []
--   result <- evaluateKontract  kuberTesnet (runSampleTest)
--   case result of
--     Left (FrameworkError _type err) -> putStrLn $ "Test failed with error: " ++ err
--     Right val -> putStrLn $ "Test succeeded with value: " ++ show val

kGroup = testGroup