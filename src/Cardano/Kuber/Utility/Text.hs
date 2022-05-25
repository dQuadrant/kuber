{-# LANGUAGE FlexibleContexts #-}
module Cardano.Kuber.Utility.Text where
import Data.Text.Conversions (FromText (fromText), ToText (toText), Base16 (Base16, unBase16), convertText)
import Data.ByteString ( ByteString )
import Data.Functor ((<&>))
import qualified Data.ByteString.Lazy as LBS


toHexString :: (FromText a1, ToText (Base16 a2)) => a2 -> a1
toHexString bs = fromText $  toText (Base16 bs )


unHexLazy :: ToText a => a -> Maybe  LBS.ByteString
unHexLazy v = convertText (toText v) <&> unBase16

unHexStrict ::  ToText a => a -> Maybe  ByteString
unHexStrict v = convertText (toText v) <&> unBase16

unHex :: (Functor f, FromText (f (Base16 b)), ToText a) => a -> f b
unHex v = convertText (toText v) <&> unBase16