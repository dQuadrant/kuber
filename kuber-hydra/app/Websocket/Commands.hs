module Websocket.Commands where 
import qualified Data.Text as T
import Websocket.Connect
import System.Environment
import Cardano.Kuber.Data.Models (UtxoModal)
import Cardano.Api
import Data.Map as Map hiding (map)
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import Cardano.Kuber.Data.Parsers
import Data.Set as Set hiding (map)

data Action =  InitializeHead
            | CommitUTxO
            | Abort
            | QueryUTxO
            | CloseHead
            | FanOut


sendMessage :: IO (T.Text -> IO ())
sendMessage = do 
    hydraIp <- getEnv "HYDRA_IP"
    hydraPort <- getEnv "HYDRA_PORT"
    webSocketProxy hydraIp (read hydraPort :: Int)


initialize :: IO ()
initialize = sendMessage >>= \send -> send (T.pack "{\"tag\": \"Init\"}")

abort :: IO ()
abort = sendMessage >>= \send -> send (T.pack "{\"tag\": \"Abort\"}")

close :: IO ()
close = sendMessage >>= \send -> send (T.pack "{\"tag\": \"Close\"}")

getUTxO :: IO ()
getUTxO = sendMessage >>= \send -> send (T.pack "{\"tag\": \"GetUTxO\"}")

-- commitUTxO :: UTxO ConwayEra -> IO ()
-- commitUTxO utxos = case utxos of
--     UTxO utxos -> Map.toList utxos

uTxOsToHydraJson :: UTxO era -> T.Text
uTxOsToHydraJson utxos = case utxos of 
    UTxO utxos -> do 
        let utxoList = Map.toList utxos
        let utxoJson = map (\(TxIn txId index, TxOut addrress value datum referenceScript) -> 
                    ()
                ) utxoList
                -- "{\"txId\": \"" <> T.pack (show txId) <> "\", \"index\": " <> T.pack (show index) <> ", \"address\": \"" <> T.pack (show addr) <> "\", \"coin\": " <> T.pack (show coin) <> "}") utxoList
        T.pack ""

-- myUTxos :: (HasChainQueryAPI a) => Kontract a w FrameworkError (UTxO era)
-- myUTxos = kQueryUtxoByAddress (Set.singleton $ addressInEraToAddressAny $ parseAddress $ T.pack "")