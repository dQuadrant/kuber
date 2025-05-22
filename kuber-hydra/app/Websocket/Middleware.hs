{-# LANGUAGE DataKinds #-}

module Websocket.Middleware where

import Cardano.Api (ConwayEra, UTxO)
import qualified Data.Aeson as A
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

errorMiddleware :: (Eq a, Num a) => a -> ServerError
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

type UVerbResponseTypes =
  '[ WithStatus 200 A.Value,
     WithStatus 201 A.Value,
     WithStatus 300 A.Value,
     WithStatus 301 A.Value,
     WithStatus 302 A.Value,
     WithStatus 303 A.Value,
     WithStatus 304 A.Value,
     WithStatus 305 A.Value,
     WithStatus 307 A.Value,
     WithStatus 400 A.Value,
     WithStatus 401 A.Value,
     WithStatus 402 A.Value,
     WithStatus 403 A.Value,
     WithStatus 404 A.Value,
     WithStatus 405 A.Value,
     WithStatus 406 A.Value,
     WithStatus 407 A.Value,
     WithStatus 409 A.Value,
     WithStatus 410 A.Value,
     WithStatus 411 A.Value,
     WithStatus 412 A.Value,
     WithStatus 413 A.Value,
     WithStatus 414 A.Value,
     WithStatus 415 A.Value,
     WithStatus 416 A.Value,
     WithStatus 417 A.Value,
     WithStatus 418 A.Value,
     WithStatus 422 A.Value,
     WithStatus 429 A.Value,
     WithStatus 500 A.Value,
     WithStatus 501 A.Value,
     WithStatus 502 A.Value,
     WithStatus 503 A.Value,
     WithStatus 504 A.Value,
     WithStatus 505 A.Value
   ]