{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    createRemoteKuberConnection network "http://localhost:8081" Nothing


exConPP = ""

localNodeConnection :: IO ChainConnectInfo
localNodeConnection = chainInfoFromEnv

printBalanceKontract :: HasChainQueryAPI api =>  Kontract api BabbageEra FrameworkError ()
printBalanceKontract=  do
    (addr ::AddressInEra BabbageEra) <- kWrapParser $ parseAddressBech32 (T.pack "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj")
    tip <- kQueryChainPoint
    (utxos ::UTxO BabbageEra) <- kQueryUtxoByAddress  (Set.singleton $ addressInEraToAddressAny addr)
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