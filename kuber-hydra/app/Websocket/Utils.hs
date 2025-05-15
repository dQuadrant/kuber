{-# LANGUAGE OverloadedStrings #-}

module Websocket.Utils where

import Cardano.Kuber.Api
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Websocket.Aeson

textToJSON :: T.Text -> Either FrameworkError A.Value
textToJSON jsonText = do
  let jsonBytes = TLE.encodeUtf8 (TL.fromStrict jsonText)
  case A.eitherDecode jsonBytes of
    Right val -> Right val
    Left _ -> Left $ FrameworkError ParserError $ "Could not parse to JSON : " <> T.unpack jsonText

jsonToText :: A.Value -> T.Text
jsonToText = TL.toStrict . TLE.decodeUtf8 . A.encode

textToLazyByteString :: T.Text -> BSL.ByteString
textToLazyByteString = BSL.fromStrict . TE.encodeUtf8

createHydraStateResponseAeson :: HeadState -> IO A.Value
createHydraStateResponseAeson hs =
  let stateText = T.pack $ case hs of
        HeadIsIdle -> "Head is Idle"
        HeadIsContested -> "Head is Contested"
        WaitingCommitments -> "Initialized and Waiting For Commitments"
        PartiallyCommitted -> "Partial Commitments Received"
        HeadIsReady -> "Open and Ready for Transactions"
        HeadIsClosed -> "Closed and Ready for Fanout"
   in pure $ A.object ["state" .= stateText]