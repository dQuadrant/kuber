
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Websocket.Commands where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models
import Cardano.Kuber.Data.Parsers
import Data.Aeson
import qualified Data.Aeson as A
import Data.ByteString.Char8 as BS8 hiding (elem, filter, head, length, map, null)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Data.Text.Encoding as T
import GHC.List
import Websocket.Aeson
import Websocket.Forwarder
import Websocket.SocketConnection
import Websocket.TxBuilder
import Websocket.Utils
import Prelude hiding (elem, length, map, null)
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Network.HTTP.Conduit (Response(responseStatus, responseBody))
import Network.HTTP.Types.Status (Status(statusCode))
import qualified Data.ByteString.Lazy.Char8 as BS8L
import qualified Data.Set as Set

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

submit :: AppConfig -> TxModal -> Bool -> IO (Either FrameworkError TxModal)
submit txm wait = do
  submitHydraTx txm wait

commitUTxO :: AppConfig -> [TxIn] -> Maybe A.Value -> Bool -> IO (Either FrameworkError TxModal)
commitUTxO appConfig txins sk submit = do

  validUtxos ::Either FrameworkError (UTxO ConwayEra) <- evaluateL1Kontract appConfig (kQueryUtxoByTxin $ Set.fromList txins)
  case validUtxos of
    Left fe -> pure $ Left fe
    Right utxoJsonSchema -> do
      unsignedTxResponse <- postCommit'  appConfig utxoJsonSchema
      let responseBs=responseBody unsignedTxResponse
      case statusCode$responseStatus unsignedTxResponse of
        200 ->
          case A.eitherDecode responseBs  of
            Left err ->do
              Prelude.putStrLn $ "err: commitUTxO (" ++ show (map renderTxIn txins) ++") ="++ show err
              pure $
                Left $
                  FrameworkError ParserError $
                    "Received: " <> BS8L.unpack responseBs
            Right (HydraCommitTx cborHex _ _) ->do
              Prelude.putStrLn "Got tx"
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
                          let txModalObject = TxModal (InAnyCardanoEra ConwayEra tx')
                          if submit
                            then do
                              submittedTxResult <- evaluateL1Kontract appConfig  (kSubmitTx (InAnyCardanoEra ConwayEra tx'))
                              case submittedTxResult of
                                Left fe -> pure $ Left fe
                                Right () -> pure $ Right txModalObject
                            else
                              pure $ Right txModalObject
        _ -> do
          Prelude.putStrLn $ "err: commitUTxO (" ++ show (map renderTxIn txins) ++") ="++ BS8L.unpack (responseBody unsignedTxResponse)
          Prelude.putStrLn $ "     utxos : " ++  BS8L.unpack (A.encode  utxoJsonSchema)
          pure $ Left $ FrameworkError {feType= TxValidationError, feMessage=BS8L.unpack (responseBody unsignedTxResponse)}


createHydraStateResponseAeson :: ContentsAndTag -> HydraStateResponse
createHydraStateResponseAeson cNTag =
  let tag = cNTag.tag
      initialContent :: (Maybe HydraHead)= case A.fromJSON cNTag.contents of
            Success v -> pure v
            _ -> Nothing
  in
    case tag of
      "Initial" -> case initialContent of
            Just hydraHead -> if Map.null  hydraHead.committed
                then  HydraStateResponse tag  "Initialized and Waiting For Commitments"
                else  HydraStateResponse tag "Partial Commitments Received"
            Nothing -> HydraStateResponse tag  "Initialized and Waiting For Commitments"
      "PartiallyCommitted" -> HydraStateResponse tag "Partial Commitments Received"
      "Idle" -> HydraStateResponse tag "Head is Idle"
      "Closed" ->  HydraStateResponse tag "Head is Closed"
      "Contested" -> HydraStateResponse tag "Head is Contested"
      "Open" ->  HydraStateResponse tag "Open and Ready for Transactions"
      _ ->  HydraStateResponse tag "Unknown hydra state tag"

decommitUTxO :: AppConfig -> [TxIn] -> Maybe A.Value -> Bool -> Bool -> IO (Either FrameworkError A.Value)
decommitUTxO hydraHost utxos sk wait submit = do
  let parsedSignKey = sk >>= parseSignKey . jsonToText
  handleHydraDecommitTx hydraHost utxos parsedSignKey wait submit
--  un initialized {"contents":{"chainState":{"recordedAt":null,"spendableUTxO":{}}},"tag":"Idle"}
getHydraState :: AppConfig -> IO (Either FrameworkError HydraStateResponse)
getHydraState hydraHost = do

  getHeadResult<- getHead  hydraHost
  let hydraHead = decode $ getHeadResult :: Maybe ContentsAndTag
      unexpectedResponseError = Left $ FrameworkError ParserError "getHydraState: Unexpected response"
  case hydraHead of
    Nothing -> do
      Prelude.putStrLn  "Head was not parsed"
      pure unexpectedResponseError
    Just hydraHead -> do
      let isCommandFailed = hydraHead.tag == T.pack "CommandFailed"
      pure $ Right $ createHydraStateResponseAeson hydraHead

      -- if isCommandFailed
      --   then do
      --     let getUTxoCommandFailedResponse = decode $ BSL.fromStrict (T.encodeUtf8 allUTxOsText) :: Maybe HydraState
      --     pure $ case getUTxoCommandFailedResponse of
      --       Nothing -> unexpectedResponseError
      --       Just hs -> do
      --         if stateTag == T.pack "Idle"
      --           then do

      --           else
      --             if stateTag == T.pack "Closed"
      --               then Right $ 
      --               else
      --                 if stateTag == T.pack "Contested"
      --                   then 

      --                   else  unexpectedResponseError
      --   else do
      --     (initHeadMessage, _) <- initialize hydraHost False
      --     let decodedInitHeadMessage = decode $ BSL.fromStrict (T.encodeUtf8 initHeadMessage) :: Maybe InitializedHeadResponse
      --     case decodedInitHeadMessage of
      --       Nothing -> pure unexpectedResponseError
      --       Just initHeadResponse -> 
      --         let hydraParties = length initHeadResponse.state.contents.parameters.parties
      --             waitingCommitments = maybe 0 length initHeadResponse.state.contents.pendingCommits
      --         in
      --           pure $ if hydraParties == waitingCommitments
      --             then do
      --               pure $ createHydraStateResponseAeson WaitingCommitments
      --             else
      --               if waitingCommitments == 0
      --                 then do
      --                   pure $ createHydraStateResponseAeson HeadIsReady
      --                 else
      --                   if hydraParties > waitingCommitments
      --                     then do
      --                       pure $ createHydraStateResponseAeson PartiallyCommitted
      --                     else  Left $ FrameworkError LibraryError "getHydraState: Unknown Head State"