{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Websocket.Commands where

import Cardano.Api
import Cardano.Api.Experimental hiding (ConwayEra)
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models (UtxoModal)
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
-- import Websocket.Aeson (ProtocolParams)

import Cardano.Ledger.Core
import Control.Exception
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 as BS8 hiding (head, map)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor
import Data.Monoid (Any)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Conversions
import Data.Text.Encoding
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Debug.Trace as Debug
import GHC.Generics
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple
import qualified Network.WebSockets as WS
import System.Environment
import Websocket.Forwarder
import Websocket.SocketConnection
import Websocket.TxBuilder
import Websocket.Utils

-- import qualified Control.Lens.Internal.Deque as Set
data HydraCommitTx = HydraCommitTx
  { cborHex :: String,
    description :: String,
    txId :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

hydraHeadMessageForwardingFailed :: T.Text
hydraHeadMessageForwardingFailed = T.pack "Failed to forward message to hydra head"

initialize :: IO T.Text
initialize = do
  sendCommandToHydraNodeSocket InitializeHead

abort :: IO T.Text
abort = do
  sendCommandToHydraNodeSocket Abort

queryUTxO :: IO T.Text
queryUTxO = do
  sendCommandToHydraNodeSocket GetUTxO

commitUTxO :: [T.Text] -> Data.Aeson.Types.Value -> Either FrameworkError Data.Aeson.Types.Value
commitUTxO utxos sk = do
  let utxoSchema = unsafePerformIO $ createUTxOSchema utxos
  case parseSignKey (jsonToText sk) of
    Nothing -> Left $ FrameworkError ParserError "Invalid signing key"
    Just parsedSk -> do
      let unsignedCommitTx = unsafePerformIO $ getHydraCommitTx utxoSchema
      case A.eitherDecode (fromStrict $ encodeUtf8 unsignedCommitTx) of
        Left err -> Left $ FrameworkError ParserError $ "Error decoding Hydra response: " <> err
        Right (HydraCommitTx cborHex _ _) -> do
          case convertText (T.pack cborHex) <&> unBase16 of
            Nothing -> Left $ FrameworkError ParserError "Invalid CBOR hex in commit transaction"
            Just bs -> do
              case deserialiseFromCBOR (AsTx AsConwayEra) bs of
                Left dc -> Left $ FrameworkError ParserError $ "Deserialization failed: " <> show dc
                Right tx -> do
                  let (txBody, hydraWitness) = getTxBodyAndWitnesses tx
                      signedTx = makeSignedTransaction (hydraWitness ++ [makeShelleyKeyWitness shelleyBasedEra txBody (WitnessPaymentKey parsedSk)]) txBody
                  let submittedTx = unsafePerformIO $ submitHandler $ kSubmitTx (InAnyCardanoEra ConwayEra signedTx)
                  case unsafePerformIO submittedTx of
                    Left fe -> Left fe
                    Right _ -> Right $ object ["tx" .= getTxId txBody]

-- commitUTxO :: [T.Text] -> Data.Aeson.Types.Value -> IO (Either FrameworkError A.Value)
-- commitUTxO utxos sk = do
--   utxoSchema <- createUTxOSchema utxos

--   case parseSignKey (jsonToText sk) of
--     Nothing -> pure $ Left $ FrameworkError ParserError "Invalid signing key"
--     Just signKey -> do
--       unsignedCommitTx <- getHydraCommitTx utxoSchema

--       case A.eitherDecode (fromStrict $ encodeUtf8 unsignedCommitTx) of
--         Left err -> pure $ Left $ FrameworkError ParserError $ "Error decoding Hydra response: " <> err
--         Right (HydraCommitTx cborHex _ _) ->
--           case unBase16 (encodeUtf8 cborHex) of
--             Nothing -> pure $ Left $ FrameworkError ParserError "Invalid CBOR hex in commit transaction"
--             Just bs -> case deserialiseFromCBOR (AsTx AsConwayEra) bs of
--               Left dc -> pure $ Left $ FrameworkError ParserError $ "Deserialization failed: " <> show dc
--               Right tx -> do
--                 let (txBody, hydraWitness) = getTxBodyAndWitnesses tx
--                     signedTx = makeSignedTransaction
--                       (hydraWitness ++ [makeShelleyKeyWitness shelleyBasedEra txBody (WitnessPaymentKey signKey)])
--                       txBody

--                 submittedTx <- submitHandler $ kSubmitTx (InAnyCardanoEra ConwayEra signedTx)
--                 case unsafePerformIO submittedTx of
--                   Left fe -> pure $ Left fe
--                   Right _ -> pure $ Right $ object ["tx" .= getTxId txBody]

decommitUTxO utxos sk = do
  utxoSchema <- createUTxOSchema utxos
  signKey <- case parseSignKey (jsonToText sk) of
    Just parsedSk -> pure parsedSk
    Nothing -> error "Failure parsing singing key"
  pure $ unsafePerformIO $ buildHydraDecommitTx utxoSchema signKey

createUTxOSchema :: [T.Text] -> IO T.Text
createUTxOSchema utxos = do
  localChain <- chainInfoFromEnv
  result <- evaluateKontract localChain (getUtxoDetails @ChainConnectInfo @ConwayEra utxos)
  case result of
    Left err -> error $ "Query failed: " <> show err
    Right res -> pure $ (T.pack . BS8.unpack . BSL.toStrict . A.encode) res

getUtxoDetails ::
  (HasChainQueryAPI a, IsTxBuilderEra era) =>
  [T.Text] ->
  Kontract a w FrameworkError (UTxO era)
getUtxoDetails utxoList = do
  parsed <-
    mapM
      ( \x -> case parseTxIn x of
          Just txin -> pure txin
          Nothing -> error $ "Error parsing TxIn: " <> T.unpack x
      )
      utxoList
  kQueryUtxoByTxin (Set.fromList parsed)

getProtocolParameters :: IO A.Value
getProtocolParameters = do
  hydraProtocolParameters <- fetch >>= \query -> query (T.pack "protocol-parameters")
  pure $ textToJSON hydraProtocolParameters

submitHandler :: Kontract ChainConnectInfo w FrameworkError r -> IO (IO (Either FrameworkError r))
submitHandler tx = do
  localChain <- chainInfoFromEnv
  pure $ evaluateKontract localChain tx