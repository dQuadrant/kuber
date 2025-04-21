{-# LANGUAGE OverloadedStrings #-}

module Websocket.Utils where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import qualified Debug.Trace as Debug
import qualified Data.ByteString as BS

-- parseProtocolParameters :: T.Text -> ProtocolParams
-- parseProtocolParameters pParamsJson = case Aeson.decode (BSL.fromStrict $ TE.encodeUtf8 pParamsJson) of
--     Just pParams -> pParams
--     Nothing -> error "Error Parsing Protocol Parameters"

textToJSON :: T.Text -> A.Value
textToJSON jsonText = do
  let jsonBytes = TLE.encodeUtf8 (TL.fromStrict jsonText)
  case A.eitherDecode jsonBytes of
    Right val -> val
    Left _ -> error ""

jsonToText :: A.Value -> T.Text
jsonToText = TL.toStrict . TLE.decodeUtf8 . A.encode

textToLazyByteString :: T.Text -> BSL.ByteString
textToLazyByteString = BSL.fromStrict . TE.encodeUtf8