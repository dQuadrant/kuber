module Main where
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable(toConsoleText, toConsoleTextNoPrefix))
import qualified Data.Text as T
import qualified Data.Set as Set
import Cardano.Kuber.Api
import Cardano.Api
import Control.Monad.IO.Class
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util

main :: IO ()
main = putStrLn "Hello, Haskell!" >> run


run :: IO ()
run = do
  (networkName,network) <- getNetworkFromEnv "NETWORK"
  kuberConn <- createRemoeKuberConnection network "http://something.com:8081" Nothing
  result <- evaluateKontract kuberConn $ do 
        (asset,amount) <-kWrapParser $ parseAssetNQuantity ( T.pack "3")
        addr <- kWrapParser $ parseAddressBech32 (T.pack "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj")  
        utxos <- kQueryUtxoByAddress (Set.singleton $ addressInEraToAddressAny addr)
        liftIO (putStrLn  $ "Your utxos : \n" ++ toConsoleText " - " utxos)
        tx<- kBuildAndSubmit (txPayTo addr (valueFromList [(asset,amount)]))
        liftIO (print tx)
  
  print result