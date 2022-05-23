{-# LANGUAGE FlexibleContexts #-}
module Cardano.Kuber.Utility.Text where
import Data.Text.Conversions (FromText (fromText), ToText (toText), Base16 (Base16, unBase16), convertText)
import Data.ByteString ( ByteString )
import Data.Functor ((<&>))


toHexString :: (FromText a1, ToText (Base16 a2)) => a2 -> a1
toHexString bs = fromText $  toText (Base16 bs )

unHex ::  ToText a => a -> Maybe  ByteString
unHex v = convertText (toText v) <&> unBase16