{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Websocket.Commands where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString.Char8 as BS8 hiding (elem, filter, head, length, map, null)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import GHC.Generics
import GHC.List
import Websocket.Aeson
import Websocket.Forwarder
import Websocket.SocketConnection
import Websocket.TxBuilder
import Websocket.Utils
import Prelude hiding (elem, length, map, null)

hydraHeadMessageForwardingFailed :: T.Text
hydraHeadMessageForwardingFailed = T.pack "Failed to forward message to hydra head"

initialize :: AppConfig -> Bool -> IO (T.Text, Int)
initialize appConfig wait = do
  sendCommandToHydraNodeSocket appConfig InitializeHead wait

abort :: AppConfig -> Bool -> IO (T.Text, Int)
abort appConfig wait = do
  sendCommandToHydraNodeSocket appConfig Abort wait

close :: AppConfig -> Bool -> IO (T.Text, Int)
close appConfig wait = do
  sendCommandToHydraNodeSocket appConfig CloseHead wait

contest :: AppConfig -> Bool -> IO (T.Text, Int)
contest appConfig wait = do
  sendCommandToHydraNodeSocket appConfig ContestHead wait

fanout :: AppConfig -> Bool -> IO (T.Text, Int)
fanout appConfig wait = do
  sendCommandToHydraNodeSocket appConfig FanOut wait

submit :: AppConfig -> TxModal -> IO (T.Text, Int)
submit txm = do
  submitHydraTx txm

commitUTxO :: AppConfig -> [TxIn] -> Maybe A.Value -> Bool -> IO (Either FrameworkError A.Value)
commitUTxO appConfig utxos sk submit = do
  utxoSchema <- createUTxOSchema utxos
  case utxoSchema of
    Left fe -> pure $ Left fe
    Right utxoJsonSchema -> do
      unsignedCommitTxOrError <- getHydraCommitTx appConfig utxoJsonSchema
      case unsignedCommitTxOrError of
        Left fe -> pure $ Left fe
        Right unsignedCommitTx ->
          case A.eitherDecode (fromStrict $ T.encodeUtf8 unsignedCommitTx) of
            Left err ->
              pure $
                Left $
                  FrameworkError ParserError $
                    "Received: " <> T.unpack unsignedCommitTx <> ". Error decoding Hydra response: " <> err
            Right (HydraCommitTx cborHex _ _) ->
              case convertText (T.pack cborHex) <&> unBase16 of
                Nothing -> pure $ Left $ FrameworkError ParserError "Invalid CBOR hex in commit transaction"
                Just bs -> do
                  case parsedTxAnyEra bs of
                    Left fe -> pure $ Left fe
                    Right tx -> do
                      let result = case sk of
                            Nothing -> Right tx
                            Just sk' -> case parseSignKey (jsonToText sk') of
                              Nothing -> Left $ FrameworkError ParserError "Invalid signing key"
                              Just parsedSk -> do
                                let (txBody, hydraWitness) = getTxBodyAndWitnesses tx
                                    signedTx = makeSignedTransaction (hydraWitness ++ [makeShelleyKeyWitness shelleyBasedEra txBody (WitnessPaymentKey parsedSk)]) txBody
                                Right signedTx
                      case result of
                        Left fe -> pure $ Left fe
                        Right tx' -> do
                          let cborHex' :: T.Text = toHexString $ serialiseToCBOR tx'
                              txObject = buildTxModalObject cborHex' (not $ null $ snd $ getTxBodyAndWitnesses tx')
                          if submit
                            then do
                              submittedTxResult <- submitHandler $ kSubmitTx (InAnyCardanoEra ConwayEra tx')
                              case submittedTxResult of
                                Left fe -> pure $ Left fe
                                Right () -> pure $ Right txObject
                            else
                              pure $ Right txObject

decommitUTxO :: AppConfig -> [TxIn] -> Maybe A.Value -> Bool -> Bool -> IO (Either FrameworkError A.Value)
decommitUTxO hydraHost utxos sk wait submit = do
  let parsedSignKey = sk >>= parseSignKey . jsonToText
  handleHydraDecommitTx hydraHost utxos parsedSignKey wait submit

getHydraState :: AppConfig -> IO (Either FrameworkError A.Value)
getHydraState hydraHost = do
  (allUTxOsText, _) <- sendCommandToHydraNodeSocket hydraHost GetUTxO False
  let allHydraUTxOs = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOsText) :: Maybe WSMessage
      unexpectedResponseError = Left $ FrameworkError ParserError "getHydraState: Unexpected response"
  case allHydraUTxOs of
    Nothing -> pure unexpectedResponseError
    Just res -> do
      let isCommandFailed = res.tag == T.pack "CommandFailed"
      if isCommandFailed
        then do
          let getUTxoCommandFailedResponse = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOsText) :: Maybe HydraState
          case getUTxoCommandFailedResponse of
            Nothing -> pure unexpectedResponseError
            Just hs -> do
              let stateTag = hs.state.tag
              if stateTag == T.pack "Idle"
                then do
                  response <- createHydraStateResponseAeson HeadIsIdle
                  pure $ Right response
                else
                  if stateTag == T.pack "Closed"
                    then do
                      response <- createHydraStateResponseAeson HeadIsClosed
                      pure $ Right response
                    else
                      if stateTag == T.pack "Contested"
                        then do
                          response <- createHydraStateResponseAeson HeadIsContested
                          pure $ Right response
                        else pure unexpectedResponseError
        else do
          (initHeadMessage, _) <- initialize hydraHost False
          let decodedInitHeadMessage = decode $ BSL.fromStrict (T.encodeUtf8 initHeadMessage) :: Maybe InitializedHeadResponse
          case decodedInitHeadMessage of
            Nothing -> pure unexpectedResponseError
            Just initHeadResponse -> do
              let hydraParties = length initHeadResponse.state.contents.parameters.parties
                  waitingCommitments = maybe 0 length initHeadResponse.state.contents.pendingCommits

              if hydraParties == waitingCommitments
                then do
                  response <- createHydraStateResponseAeson WaitingCommitments
                  pure $ Right response
                else
                  if waitingCommitments == 0
                    then do
                      response <- createHydraStateResponseAeson HeadIsReady
                      pure $ Right response
                    else
                      if hydraParties > waitingCommitments
                        then do
                          respone <- createHydraStateResponseAeson PartiallyCommitted
                          pure $ Right respone
                        else pure $ Left $ FrameworkError LibraryError "getHydraState: Unknown Head State"