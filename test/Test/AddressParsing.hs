{-# LANGUAGE FlexibleContexts #-}
module Test.AddressParsing where

import qualified Data.Text as T
import Cardano.Api
import Cardano.Kuber.Util
import Data.Text.Conversions
import Data.ByteString (ByteString)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash))
import Data.Functor ((<&>))
import Plutus.V2.Ledger.Api (fromBuiltin)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Utils.addrInEraToPkh" [
    testCase "should parse full shelley base address" (tAddressToPkh  "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj" @?= Just "f735cdaa2ae360ce67e27cd2f6038a44c77bf3ede94bd144674d34e6")
  ]


tAddressToPkh  :: String ->  Maybe String
tAddressToPkh address = deserialiseAddress (AsAddressInEra AsAlonzoEra) (T.pack address) >>= addrInEraToPkh <&> (\(PubKeyHash pkh ) -> toHexString $ fromBuiltin pkh )
    where
      toHexString :: (FromText a1, ToText (Base16 a2)) => a2 -> a1
      toHexString bs = fromText $  toText (Base16 bs )

      unHex ::  ToText a => a -> Maybe  ByteString
      unHex v = convertText (toText v) <&> unBase16