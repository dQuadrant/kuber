{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Websocket.Commands where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Parsers
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.ByteString.Char8 as BS8 hiding (elem, filter, head, map, null)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Map as M hiding (filter, null)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import GHC.Generics
import GHC.List
import Websocket.Forwarder
import Websocket.SocketConnection
import Websocket.TxBuilder
import Websocket.Utils
import Prelude hiding (null)

data HydraCommitTx = HydraCommitTx
  { cborHex :: String,
    description :: String,
    txId :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

hydraHeadMessageForwardingFailed :: T.Text
hydraHeadMessageForwardingFailed = T.pack "Failed to forward message to hydra head"

initialize :: Bool -> IO (T.Text, Int)
initialize wait = do
  sendCommandToHydraNodeSocket InitializeHead wait

abort :: Bool -> IO (T.Text, Int)
abort wait = do
  sendCommandToHydraNodeSocket Abort wait

close :: Bool -> IO (T.Text, Int)
close wait = do
  sendCommandToHydraNodeSocket CloseHead wait

contest :: Bool -> IO (T.Text, Int)
contest wait = do
  sendCommandToHydraNodeSocket ContestHead wait

fanout :: Bool -> IO (T.Text, Int)
fanout wait = do
  sendCommandToHydraNodeSocket FanOut wait

queryUTxO :: Maybe T.Text -> Maybe T.Text -> IO (Either FrameworkError A.Value)
queryUTxO address txin = do
  (allUTxOsText, _) <- sendCommandToHydraNodeSocket GetUTxO False
  let allHydraUTxOs = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOsText) :: Maybe HydraGetUTxOResponse
  case allHydraUTxOs of
    Nothing -> return $ Left $ FrameworkError ParserError "queryUTxO: Error parsing Hydra UTxOs"
    Just parsedHydraUTxOs -> do
      case txin of
        Just tin -> do
          case parseTxIn tin of
            Just _ -> do
              let hydraUTxOs = utxo parsedHydraUTxOs
                  missing = filter (`M.notMember` hydraUTxOs) [tin]
              if not (null missing)
                then return $ Left $ FrameworkError ParserError $ "Missing UTxOs in Hydra: " ++ show missing
                else do
                  let hydraTxInFilteredUTxOs = M.filterWithKey (\k _ -> k == tin) hydraUTxOs
                      converted = KM.fromList [(K.fromText k, v) | (k, v) <- M.toList hydraTxInFilteredUTxOs]
                  return $ Right $ A.Object converted
            Nothing -> pure $ Left $ FrameworkError ParserError $ "queryUTxO : Unable to parse TxIn: " <> T.unpack tin
        Nothing ->
          case address of
            Just addr ->
              case parseAddress @ConwayEra addr of
                Just _ -> do
                  let hydraUTxOs = utxo parsedHydraUTxOs
                      hydraAddressFilteredUTxOs =
                        M.filter
                          ( \val ->
                              case val of
                                A.Object obj ->
                                  case KM.lookup "address" obj of
                                    Just (A.String a) -> a == addr
                                    _ -> False
                                _ -> False
                          )
                          hydraUTxOs
                      converted = KM.fromList [(K.fromText k, v) | (k, v) <- M.toList hydraAddressFilteredUTxOs]
                  return $ Right $ A.Object converted
                Nothing -> pure $ Left $ FrameworkError ParserError $ "queryUTxO : Unable to parse address: " <> T.unpack addr
            Nothing -> pure $ Right $ A.toJSON $ utxo parsedHydraUTxOs

commitUTxO :: [T.Text] -> A.Value -> IO (Either FrameworkError A.Value)
commitUTxO utxos sk = do
  utxoSchema <- createUTxOSchema utxos
  case parseSignKey (jsonToText sk) of
    Nothing -> pure $ Left $ FrameworkError ParserError "Invalid signing key"
    Just parsedSk -> do
      unsignedCommitTxOrError <- getHydraCommitTx utxoSchema
      case unsignedCommitTxOrError of
        Right unsignedCommitTx -> do
          case A.eitherDecode (fromStrict $ T.encodeUtf8 unsignedCommitTx) of
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

decommitUTxO :: [T.Text] -> Data.Aeson.Types.Value -> Bool -> IO (Either FrameworkError A.Value)
decommitUTxO utxos sk wait = do
  signKey <- case parseSignKey (jsonToText sk) of
    Just parsedSk -> pure parsedSk
    Nothing -> error "Failure parsing singing key"
  submitHydraDecommitTx utxos signKey wait

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