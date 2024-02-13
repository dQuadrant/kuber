module Main where
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable(toConsoleText, toConsoleTextNoPrefix))
import qualified Data.Text as T
import qualified Data.Set as Set
import Cardano.Kuber.Api
import Cardano.Api
import Control.Monad.IO.Class
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util

remoteKuberConnection :: IO RemoteKuberConnection
remoteKuberConnection = do 
    (networkName,network) <- getNetworkFromEnv "NETWORK"    
    createRemoteKuberConnection network "http://172.31.6.14:8081" Nothing

localNodeConnection :: IO ChainConnectInfo
localNodeConnection = chainInfoFromEnv

printBalanceKontract :: HasChainQueryAPI api =>  Kontract api w FrameworkError ()
printBalanceKontract=  do
    addr <- kWrapParser $ parseAddressBech32 (T.pack "addr1w904uqjsfjeaqgrzq60y8aajuqhcaa3ar4jp227k5w2v8hswf38tq")
    tip <- kQueryChainPoint
    utxos <- kQueryUtxoByAddress (Set.singleton $ addressInEraToAddressAny addr)
    liftIO $ do 
        putStrLn ("Chain is at " ++ (case tip of
            ChainPointAtGenesis -> "Genesis"
            ChainPoint (SlotNo sn) ha -> "SlotNo:" ++ show sn ++ " BlockHeaderHash:" ++ T.unpack (serialiseToRawBytesHexText ha)) )
        putStrLn  $ "Your utxos : \n" ++ toConsoleText " - " utxos

main :: IO ()
main = do
  kuberConn <- remoteKuberConnection
  result <- evaluateKontract  kuberConn printBalanceKontract
  case result of 
    Left e -> putStrLn $ "Unexpected error evaluating printBalance kontract:\n  "++ show e
    _ -> pure ()