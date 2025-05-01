{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Websocket.Commands where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Parsers
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString.Char8 as BS8 hiding (head, map)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Conversions
import Data.Text.Encoding
import qualified Debug.Trace as Debug
import GHC.Generics
import Websocket.Forwarder
import Websocket.SocketConnection
import Websocket.TxBuilder
import Websocket.Utils

data HydraCommitTx = HydraCommitTx
  { cborHex :: String,
    description :: String,
    txId :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

hydraHeadMessageForwardingFailed :: T.Text
hydraHeadMessageForwardingFailed = T.pack "Failed to forward message to hydra head"

initialize :: IO (T.Text, Int)
initialize = do
  sendCommandToHydraNodeSocket InitializeHead

abort :: IO (T.Text, Int)
abort = do
  sendCommandToHydraNodeSocket Abort

close :: IO (T.Text, Int)
close = do
  sendCommandToHydraNodeSocket CloseHead

fanout :: IO (T.Text, Int)
fanout = do
  sendCommandToHydraNodeSocket FanOut

queryUTxO :: IO (T.Text, Int)
queryUTxO = do
  sendCommandToHydraNodeSocket GetUTxO

commitUTxO :: [T.Text] -> A.Value -> IO (Either FrameworkError A.Value)
commitUTxO utxos sk = do
  utxoSchema <- createUTxOSchema utxos
  case parseSignKey (jsonToText sk) of
    Nothing -> pure $ Left $ FrameworkError ParserError "Invalid signing key"
    Just parsedSk -> do
      unsignedCommitTxOrError <- getHydraCommitTx utxoSchema
      case unsignedCommitTxOrError of
        Right unsignedCommitTx -> do
          case A.eitherDecode (fromStrict $ encodeUtf8 unsignedCommitTx) of
            Left err ->
              pure $
                Left $
                  FrameworkError ParserError $
                    "Received: " <> T.unpack unsignedCommitTx <> ". Error decoding Hydra response: " <> err
            Right (HydraCommitTx cborHex _ _) ->
              case convertText (T.pack cborHex) <&> unBase16 of
                Nothing -> pure $ Left $ FrameworkError ParserError "Invalid CBOR hex in commit transaction"
                Just bs ->
                  case deserialiseFromCBOR (AsTx AsConwayEra) bs of
                    Left dc -> pure $ Left $ FrameworkError ParserError $ "Deserialization failed: " <> show dc
                    Right tx -> do
                      let (txBody, hydraWitness) = getTxBodyAndWitnesses tx
                          signedTx =
                            makeSignedTransaction
                              (hydraWitness ++ [makeShelleyKeyWitness shelleyBasedEra txBody (WitnessPaymentKey parsedSk)])
                              txBody
                      submittedTxResult <- submitHandler $ kSubmitTx (InAnyCardanoEra ConwayEra signedTx)
                      case submittedTxResult of
                        Left fe -> pure $ Left fe
                        Right () -> pure $ Right $ object ["tx" .= getTxId txBody]
        Left fe -> pure $ Left fe

decommitUTxO :: [T.Text] -> Data.Aeson.Types.Value -> IO (Either FrameworkError A.Value)
decommitUTxO utxos sk = do
  signKey <- case parseSignKey (jsonToText sk) of
    Just parsedSk -> pure parsedSk
    Nothing -> error "Failure parsing singing key"
  submitHydraDecommitTx utxos signKey

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

getProtocolParameters :: IO (Either FrameworkError A.Value)
getProtocolParameters = do
  hydraProtocolParameters <- fetch >>= \query -> query (T.pack "protocol-parameters")
  pure $ textToJSON hydraProtocolParameters