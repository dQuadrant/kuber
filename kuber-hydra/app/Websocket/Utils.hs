{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Websocket.Utils where

import Cardano.Api
    ( ToJSON,
      ConwayEra,
      Tx,
      InAnyCardanoEra(InAnyCardanoEra),
      CardanoEra(ConwayEra),
      TxIn(..),
      UTxO(UTxO),
      TxIx(TxIx),
      AddressInEra,
      IsShelleyBasedEra(shelleyBasedEra),
      AddressAny(..),
      serialiseToRawBytesHexText,
      shelleyAddressInEra,
      byronAddressInEra )
import Cardano.Kuber.Api
    ( FrameworkError(FrameworkError),
      ErrorType(ParserError, NodeQueryError),
      HasChainQueryAPI(kQueryUtxoByTxin),
      IsTxBuilderEra,
      Kontract,
      evaluateKontract )
import Cardano.Kuber.Data.Parsers
    ( parseRawTxInAnyEra, parseTxIn, parseAddress )
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Either ( rights, lefts )
import Data.List ( nub, (\\) )
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

textToJSON :: T.Text -> Either FrameworkError A.Value
textToJSON jsonText = do
  let jsonBytes = TLE.encodeUtf8 (TL.fromStrict jsonText)
  case A.eitherDecode jsonBytes of
    Right val -> Right val
    Left _ -> Left $ FrameworkError ParserError $ "Could not parse to JSON : " <> T.unpack jsonText

bytestringToJSON :: BSL.ByteString -> Either FrameworkError A.Value
bytestringToJSON bs = case A.eitherDecode bs of
  Right val -> Right val
  Left _ -> Left $ FrameworkError ParserError $ "Could not parse to JSON : " <> BS8.unpack (BSL.toStrict bs)

eitherObjectToJSON :: (ToJSON a) => Either FrameworkError a -> IO (Either FrameworkError A.Value)
eitherObjectToJSON obj = case obj of
  Left fe -> pure $ Left fe
  Right obj' -> case bytestringToJSON $ A.encode obj' of
    Left fe -> pure $ Left fe
    Right val -> pure $ Right val

jsonToText :: A.Value -> T.Text
jsonToText = TL.toStrict . TLE.decodeUtf8 . A.encode

textToLazyByteString :: T.Text -> BSL.ByteString
textToLazyByteString = BSL.fromStrict . TE.encodeUtf8

-- createHydraStateResponseAeson :: HeadState -> HydraStateResponse
-- createHydraStateResponseAeson hs =
--   let stateText = T.pack $ case hs of
--         HeadIsIdle -> "Head is Idle"
--         HeadIsContested -> "Head is Contested"
--         WaitingCommitments -> "Initialized and Waiting For Commitments"
--         PartiallyCommitted -> "Partial Commitments Received"
--         HeadIsReady -> "Open and Ready for Transactions"
--         HeadIsClosed -> "Head is Closed"
--    in  HydraStateResponse {stateText}

parsedTxAnyEra :: BS8.ByteString -> Either FrameworkError (Tx ConwayEra)
parsedTxAnyEra bs =
  case parseRawTxInAnyEra bs of
    Just (InAnyCardanoEra ConwayEra tx) -> Right (tx :: Tx ConwayEra)
    Just (InAnyCardanoEra _ _) -> Left $ FrameworkError ParserError "Unexpected era, expected ConwayEra"
    Nothing -> Left $ FrameworkError ParserError "Error parsing transaction cbor"

createUTxOSchema ::  HasChainQueryAPI api  => api -> [TxIn] -> IO (Either FrameworkError T.Text)
createUTxOSchema api utxos = do
  let utxoKontract  :: HasChainQueryAPI api  =>  Kontract api w FrameworkError (UTxO ConwayEra)
      utxoKontract = getUtxoDetails  utxos
  result <- evaluateKontract  api  utxoKontract
  case result of
    Left err -> pure $ Left err
    Right res -> case res of
      (UTxO txinMap) -> do
        let txInKeys = M.keys txinMap
            allTxInsPresent = utxos `isSubsetOf` txInKeys
        pure $ if allTxInsPresent
          then
             Right $ (T.pack . BS8.unpack . BSL.toStrict . A.encode) res
          else
            Left $ FrameworkError NodeQueryError $ "These TxIns were not found on chain : " <> show (utxos `notPresentIn` txInKeys)

notPresentIn :: (Eq a) => [a] -> [a] -> [a]
notPresentIn a b = filter (`notElem` b) a

getUtxoDetails ::
  (HasChainQueryAPI a, IsTxBuilderEra era) =>
  [TxIn] ->
  Kontract a w FrameworkError (UTxO era)
getUtxoDetails utxoList = do
  kQueryUtxoByTxin (Set.fromList utxoList)

listOfTextToTxIn :: [T.Text] -> IO (Either FrameworkError [TxIn])
listOfTextToTxIn txins = do
  let parsedList =
        map
          ( \t -> case parseTxIn t of
              Just txin -> Right txin
              Nothing -> Left t
          )
          txins
      (correctTxIns, incorrectTexts) = (rights parsedList, lefts parsedList)
  if not (null incorrectTexts)
    then
      pure $ Left $ FrameworkError ParserError $ "Could not parse the following to TxIn: " <> show incorrectTexts
    else
      pure $ Right correctTxIns

listOfTxInToText :: [TxIn] -> [T.Text]
listOfTxInToText = map (\(TxIn hash (TxIx num)) -> serialiseToRawBytesHexText hash <> "#" <> T.pack (show $ toInteger num))

listOfTextToAddressInEra :: [T.Text] -> IO (Either FrameworkError [AddressInEra ConwayEra])
listOfTextToAddressInEra textAddresses = do
  let parsedList =
        map
          ( \a -> case parseAddress @ConwayEra a of
              Just addr -> Right addr
              Nothing -> Left a
          )
          textAddresses
      (correctAddresses, incorrectTexts) = (rights parsedList :: [AddressInEra ConwayEra], lefts parsedList)
  if not (null incorrectTexts)
    then
      pure $ Left $ FrameworkError ParserError $ "Could not parse the following to Address: " <> show incorrectTexts
    else
      pure $ Right correctAddresses

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isSubsetOf b a =
  let missing = nub b \\ a
   in null missing

parseUTxO :: BSL.ByteString -> Either FrameworkError (UTxO ConwayEra)
parseUTxO bs = case A.decode bs :: Maybe (UTxO ConwayEra) of
  Just x -> Right x
  Nothing -> Left $ FrameworkError ParserError "parseUTxO: Failulre parsing UTxO Json schema to UTxO ConwayEra"

utxoFromUTxOMap :: [M.Map T.Text A.Value] -> [Either FrameworkError (UTxO ConwayEra)]
utxoFromUTxOMap = map ((parseUTxO . A.encode) . (\x -> KM.fromList [(K.fromText k, v) | (k, v) <- M.toList x]))

listOfUTxOToSingleUTxO :: [UTxO ConwayEra] -> UTxO ConwayEra
listOfUTxOToSingleUTxO utxos =
  UTxO $ mconcat [m | UTxO m <- utxos]

setOfAddressAnyToAddressesInConwayEra :: Set.Set AddressAny -> [AddressInEra ConwayEra]
setOfAddressAnyToAddressesInConwayEra addrs =
  map
    ( \anyAddress -> case anyAddress of
        AddressShelley addrShelley -> shelleyAddressInEra shelleyBasedEra addrShelley
        AddressByron addrByron -> byronAddressInEra addrByron
    )
    (Set.toList addrs)