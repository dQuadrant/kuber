{- |
  A Wai middleware that uniformly structures errors within a servant application.
  The library assumes all HTTP responses with status codes between @4xx@ and @5xx@ while
  lacking an @HTTP content-type@ are error responses. This assumption is derived
  from servant server error handling implementation.

  The formatting and structuring of errors rest on the implementation of 'HasErrorBody' class instances.
  It's class parameters are a content-type eg @JSON@ or @PlainText@ and a type-level list of
  @options@ e.g @'["error", "status"]@. The library offers instances for 'JSON' and 'PlainText' content-types.

  ==Sample usage with servant

  ===A typical servant application is usually of this form:

  @
  main :: IO ()
  main = run 8001 (serve proxyApi handlers)
  @

  ===With servant-errors as an error processing middleware:

  @
  main :: IO ()
  main = run 8001
     $ errorMw \@JSON \@\'["error", "status"]
     -- ^ Structures error response as JSON objects
     -- with @error@ and @status@ strings as error object field keys
     -- note they can be changed to any other preferred strings.
     $ serve proxyApi handlers
  @
-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Kuber.Server.ServantError
  ( -- * Error Middleware
    errorMw
  , errorMwDefJson

  -- * HasErrorBody class
  , HasErrorBody (..)

  -- * Helper functions and data types
  , ErrorMsg (..)
  , StatusCode (..)
  , ErrorLabels (..)
  , getErrorLabels
  , encodeAsJsonError
  , encodeAsPlainText
  )where

import Prelude.Compat
import Data.Aeson (Value (..), encode)
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson.KeyMap as H
import qualified Data.Aeson.Key as A
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Media as M
import Network.HTTP.Types (Header, Status (..), hContentType)
import Network.Wai (Middleware, Response, responseHeaders, responseLBS, responseStatus,
                    responseToStream)
import Servant.API.ContentTypes (Accept (..), JSON, PlainText)

-- | 'StatusCode' holds HTTP error status code
newtype StatusCode = StatusCode { unStatusCode :: Int }
  deriving (Eq, Ord, Show)

-- | 'ErrorMsg' holds HTTP error response body message
newtype ErrorMsg = ErrorMsg { unErrorMsg :: T.Text }
  deriving Show

-- | 'ErrorLabels' is a configuration for holding error response labels
data ErrorLabels = ErrorLabels
  { errName       :: T.Text
  , errStatusName :: T.Text
  }

-- | The 'HasErrorBody' class is used for structuring servant error responses.
--
-- @ctyp@ is an HTTP content-type with an 'Accept' class instance. eg @JSON@
--
-- @opts@ is a type level list for customising error and status labels.
--
-- For example:
-- @'["error-message", "status-code"]@
--
-- When @opts@ is left as an Empty type level list, it default's to a type list of these values:
-- @'["error", "status"]@ for the library provided 'JSON' and 'PlainText' instances.
--
class Accept ctyp => HasErrorBody (ctyp :: Type) (opts :: [Symbol]) where
  -- | 'encodeError' formats error response.
  -- The @opts@ type level list in the class definition is used by the 'getErrorLabels' function
  -- to obtain error labels which are subsequently used in implementing @encodeError@ for class instances
  encodeError :: StatusCode -> ErrorMsg -> LB.ByteString

instance  (KnownSymbol errLabel, KnownSymbol statusLabel)
  => HasErrorBody JSON '[errLabel, statusLabel] where
    encodeError = encodeAsJsonError (getErrorLabels @errLabel @statusLabel)

instance HasErrorBody JSON '[] where
  encodeError = encodeError @JSON @["error", "status"]

instance  (KnownSymbol errLabel, KnownSymbol statusLabel)
  => HasErrorBody PlainText '[errLabel, statusLabel] where
    encodeError = encodeAsPlainText (getErrorLabels @errLabel @statusLabel)

instance HasErrorBody PlainText '[] where
  encodeError = encodeError @PlainText @["error", "status"]

-- | 'errorMwDefJson' is a convenience pre-configured function for middleware
-- that encodes error responses as @JSON@ objects using @error@ and @status@
-- for a @JSON object@ key fields
--
-- A resulting response may look like this:
-- @\{ error: \"failed to decode request body\", status: 400 \}@
--
errorMwDefJson :: Middleware
errorMwDefJson = errorMw @JSON @'[]

-- | 'errorMw' functions provides "Network.Wai" middleware for formatting error responses
-- within a servant application.
-- Note that this function expects you to have @TypeApplications@ extension enabled
--
-- > errorMw @JSON @'[ "error", "status"]
--
errorMw :: forall ctyp opts. HasErrorBody ctyp opts => Middleware
errorMw baseApp req respond =
  baseApp req $ \ response -> do
     let status      = responseStatus response
         mcontentType = getContentTypeHeader response
     case (status, mcontentType) of
       (Status code _, Nothing) | code >= 400 && code < 600 ->
         newResponse @ctyp @opts status response >>= respond
       _                                     -> respond response
  where
    getContentTypeHeader :: Response -> Maybe Header
    getContentTypeHeader = find ((hContentType ==) . fst) . responseHeaders


-- | 'newResponse' creates new API route 'Response' content based on a 'HasErrorBody' instance
--
-- In the event that the original error response has an empty error message body e.g. a 404 error.
-- The error status message is used as the error message.
newResponse
  :: forall ctyp opts . HasErrorBody ctyp opts
  => Status
  -> Response
  -> IO Response
newResponse status@(Status code statusMsg) response = do
  body <- responseBody response
  let oldHeaders = responseHeaders response
  let newHeaders = (hContentType,  M.renderHeader $ contentType (Proxy @ctyp)) : oldHeaders
      content = ErrorMsg . cs $ if body == mempty then statusMsg else body
      newContent = encodeError @ctyp @opts (StatusCode code) content
  return $ responseLBS status newHeaders newContent

-- | 'responseBody' extracts response body from the servant server response.
responseBody :: Response -> IO B.ByteString
responseBody res =
  let (_status, _headers, streamBody) = responseToStream res in
  streamBody $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    cs . toLazyByteString <$> readIORef content

{-------------------------------------------------------------------------------
  Helper functions for defining instances
-------------------------------------------------------------------------------}

-- | 'encodeAsJsonError' formats error response into 'JSON' encoded string.
-- Its used in the library provided 'HasErrorBody' /JSON/ instance
encodeAsJsonError :: ErrorLabels -> StatusCode -> ErrorMsg -> LB.ByteString
encodeAsJsonError ErrorLabels {..} code content =
  encode $ Object
         $ H.fromList
           [ ( A.fromText errName, String $ unErrorMsg content)
           , (A.fromText errStatusName, Number $ toScientific code )
           ]
   where
     toScientific :: StatusCode -> Scientific
     toScientific = fromInteger . fromIntegral @_ @Integer . unStatusCode

-- | 'encodeAsPlainText' formats error response into 'PlainText' string.
-- its used in the library provided 'HasErrorBody' /PlainText/ class instance
encodeAsPlainText :: ErrorLabels -> StatusCode -> ErrorMsg -> LB.ByteString
encodeAsPlainText ErrorLabels {..} code content =
  cs $  errName
     <> unErrorMsg content
     <> errStatusName
     <> cs (show $ unStatusCode code)

-- | 'getErrorLabels' is used to tranform type level list options provided via the
-- 'HasErrorBody' class into an 'ErrorLabels' data type.
--
-- 'ErrorLabels' is used with the error formatting and encoding
-- functions used in \HasErrorBody\ class.
getErrorLabels
  :: forall errLabel statusLabel .(KnownSymbol errLabel, KnownSymbol statusLabel)
  => ErrorLabels
getErrorLabels = ErrorLabels (label (Proxy @errLabel)) (label (Proxy @statusLabel))
  where
    label :: KnownSymbol t => Proxy t -> T.Text
    label proxy = cs $ symbolVal proxy