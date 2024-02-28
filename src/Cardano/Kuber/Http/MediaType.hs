{-#LANGUAGE OverloadedStrings#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Cardano.Kuber.Http.MediaType
where

import Network.HTTP.Media ((//), (/:), MediaType)
import Servant.API.ContentTypes (Accept (contentType,contentTypes), MimeUnrender (mimeUnrender), MimeRender (mimeRender))
import GHC.Base (NonEmpty ((:|)))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Cardano.Binary (FromCBOR)
import qualified Cardano.Binary as CBOR
import Data.Text.Conversions (Base16(Base16), convertText)
import Data.Data (Proxy (Proxy))
import Data.ByteString (ByteString)
import Data.Proxy (asProxyTypeOf)
import qualified Data.ByteString.Lazy
import Cardano.Kuber.Data.Models (SubmitTxModal(SubmitTxModal), TxModal (TxModal))
import Data.ByteString.Lazy (fromStrict)
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra (bAsEra))
import Cardano.Api.Shelley (AsType(..))
import Cardano.Api (InAnyCardanoEra(..), Tx, SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR), CardanoEra (..))
import qualified Data.ByteString.Lazy as BS
import Cardano.Kuber.Data.Parsers (parseRawTxInAnyEra)

data AnyTextType = AnyTextType

instance Accept AnyTextType where
   contentTypes _ =  "text" // "plain" :| [  "text" // "*", "text" // "plain" /: ("charset", "utf-8") ]

instance  MimeUnrender AnyTextType String where
   mimeUnrender _ bs = Right  $  BS8.unpack  $ BSL.toStrict bs

instance  MimeUnrender AnyTextType TL.Text where
    mimeUnrender _ bs = Right  $ TL.decodeUtf8 bs

instance  MimeUnrender AnyTextType T.Text where
    mimeUnrender :: Proxy AnyTextType -> Data.ByteString.Lazy.ByteString -> Either String T.Text
    mimeUnrender _ bs = Right  $ T.decodeUtf8  $ BSL.toStrict bs

data CBORBinary =CBORBinary

instance Accept CBORBinary where
   contentTypes _ =  "octet" // "stream" :| [  "application" // "cbor"]

instance IsTxBuilderEra era => MimeUnrender  CBORBinary  ( Tx era ) where
    mimeUnrender  _ bs = case deserialiseFromCBOR (AsTx bAsEra) ( Data.ByteString.Lazy.toStrict bs) of
        Left e -> Left $ "Tx string: Invalid CBOR format : " ++ show e
        Right tx -> pure  tx

instance  MimeUnrender  CBORBinary  TxModal where
    mimeUnrender  proxy bs = do
      case parseRawTxInAnyEra ( BS.toStrict bs) of
         Just result -> pure $ TxModal result
         Nothing -> Left "Tx string: Invalid CBOR format "

instance  MimeRender CBORBinary TxModal where
  mimeRender p (TxModal (InAnyCardanoEra era tx)) = case era of
    ShelleyEra -> fromStrict $ serialiseToCBOR tx
    AllegraEra -> fromStrict $ serialiseToCBOR tx
    MaryEra -> fromStrict $ serialiseToCBOR tx
    AlonzoEra -> fromStrict $ serialiseToCBOR tx
    BabbageEra -> fromStrict $ serialiseToCBOR tx
    ConwayEra -> fromStrict $ serialiseToCBOR tx
   -- fromStrict $  serialiseToCBOR tx



instance MimeUnrender CBORBinary  SubmitTxModal where
   mimeUnrender  proxy bs  = do
         (TxModal result) <- mimeUnrender proxy bs
         pure $ SubmitTxModal result Nothing


data CBORText =CBORText

instance Accept CBORText where
   contentTypes :: Proxy CBORText -> NonEmpty MediaType
   contentTypes _ =  "text" // "plain" :| [ "text" // "cbor" , "text" //"hex", "text" // "plain" /: ("charset", "utf-8") ]

instance (MimeUnrender CBORBinary a) => MimeUnrender CBORText a where
  mimeUnrender  _ bs =  case convertText (BS8.unpack $ BSL.toStrict bs) of
      Nothing -> Left "Expected hex encoded CBOR string"
      Just (Base16 bs) -> mimeUnrender (Proxy  :: (Proxy CBORBinary)) bs

instance (MimeRender CBORBinary a) => MimeRender CBORText a where
   mimeRender :: MimeRender CBORBinary a => Proxy CBORText -> a -> Data.ByteString.Lazy.ByteString
   mimeRender proxy  obj= mimeRender  (Proxy  :: (Proxy CBORBinary)) obj