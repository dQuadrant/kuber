module Websocket.Middleware where

import Network.HTTP.Types
import Network.Wai
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS8

hPragma :: HeaderName
hPragma = CI.mk (BS8.pack "Pragma")

hExpires :: HeaderName
hExpires = CI.mk (BS8.pack "Expires")

noCacheMiddleware :: Middleware
noCacheMiddleware app req sendResponse =
  app req $ \res ->
    let headers =
          [ (hCacheControl, BS8.pack "no-store, no-cache, must-revalidate, max-age=0"),
            (hPragma, BS8.pack "no-cache"),
            (hExpires, BS8.pack "0")
          ]
        res' = mapResponseHeaders (++ headers) res
     in sendResponse res'