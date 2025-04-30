{-# LANGUAGE OverloadedStrings #-}

module Websocket.Utils where

import Cardano.Kuber.Api
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
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

jsonToText :: A.Value -> T.Text
jsonToText = TL.toStrict . TLE.decodeUtf8 . A.encode

textToLazyByteString :: T.Text -> BSL.ByteString
textToLazyByteString = BSL.fromStrict . TE.encodeUtf8