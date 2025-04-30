module Websocket.Middleware where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types
import Network.Wai
import Servant

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

errorMiddleware status
  | status == 300 = err300
  | status == 301 = err301
  | status == 302 = err302
  | status == 303 = err303
  | status == 304 = err304
  | status == 305 = err305
  | status == 307 = err307
  | status == 400 = err400
  | status == 401 = err401
  | status == 402 = err402
  | status == 403 = err403
  | status == 404 = err404
  | status == 405 = err405
  | status == 406 = err406
  | status == 407 = err407
  | status == 409 = err409
  | status == 410 = err410
  | status == 411 = err411
  | status == 412 = err412
  | status == 413 = err413
  | status == 414 = err414
  | status == 415 = err415
  | status == 416 = err416
  | status == 417 = err417
  | status == 418 = err418
  | status == 422 = err422
  | status == 429 = err429
  | status == 500 = err500
  | status == 501 = err501
  | status == 502 = err502
  | status == 503 = err503
  | status == 504 = err504
  | status == 505 = err505
  | otherwise = err500