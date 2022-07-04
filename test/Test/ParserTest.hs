{-# LANGUAGE FlexibleContexts #-}
module Test.ParserTest where

import qualified Data.Text as T
import Cardano.Api
import Cardano.Kuber.Util hiding (toHexString)
import Data.Text.Conversions
import Data.ByteString (ByteString)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash))
import Data.Functor ((<&>))
import Plutus.V2.Ledger.Api (fromBuiltin)
import Test.Tasty.HUnit (testCase, (@?=))
import Cardano.Kuber.Data.Parsers
import qualified Debug.Trace as Debug
import qualified Data.ByteString as BS
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable(toConsoleText, toConsoleTextNoPrefix))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Cardano.Ledger.Shelley.API (ScriptHash(ScriptHash))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Utils.addrInEraToPkh" [
      baseAddressToPkh
  ,  parseutxoWithOnlyLovelace
  ,  parseUtxoWithAsset
  , parseAssetIdTest

  ]


tAddressToPkh  :: String ->  Maybe String
tAddressToPkh address = deserialiseAddress (AsAddressInEra AsBabbageEra) (T.pack address) >>= addrInEraToPkh <&> (\(PubKeyHash pkh ) -> toHexString $ fromBuiltin pkh )


baseAddressToPkh :: TestTree
baseAddressToPkh = testCase "should parse full shelley base address" (tAddressToPkh  "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj"
                                                            @?= Just "f735cdaa2ae360ce67e27cd2f6038a44c77bf3ede94bd144674d34e6")

parseutxoWithOnlyLovelace:: TestTree
parseutxoWithOnlyLovelace=testCase "should parse utxo with only lovelace output" $
  (parseUtxo (T.pack "828258208d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c05420282583901538d169e31bec9d9903f53cc2fce8fffc6d61b30298976bb20f48970f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b1a02addd3a")
    <&> toConsoleTextNoPrefix ) @?= Just "8d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c0542#2\t:\t44.948794 Ada"

parseUtxoWithAsset :: TestTree
parseUtxoWithAsset =  testCase  "should parse utxo with asset" $
      (parseUtxo (T.pack  "828258208d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c05420682583901538d169e31bec9d9903f53cc2fce8fffc6d61b30298976bb20f48970f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b821a002c3268a1581c4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36a14c466c6f77657279566964656f01")
        <&> toConsoleTextNoPrefix ) @?= Just "8d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c0542#6\t:\t2.896488 Ada +1 4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36.FloweryVideo"

parseAssetIdTest :: TestTree
parseAssetIdTest = testCase " should parse with dot" $ (parseAssetId (T.pack " 4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36.Flowery. Video\n ")
   @?= Just (AssetId  ( forceRight $  deserialiseFromRawBytesHex AsPolicyId  $ BS8.pack "4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36") (AssetName $ BS8.pack "Flowery. Video" ))
  )

parseAssetIdHex :: TestTree
parseAssetIdHex = testCase " should parse assetId Hex" $ (parseAssetId (T.pack " 4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d3604f59d3d36\n ")
   @?= Just (AssetId  ( forceRight $  deserialiseFromRawBytesHex AsPolicyId  $ BS8.pack "4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36") ( forceRight $  deserialiseFromRawBytesHex AsAssetName  $ BS8.pack "04f59d3d36" ))
  )


toHexString :: (FromText a1, ToText (Base16 a2)) => a2 -> a1
toHexString bs = fromText $  toText (Base16 bs )


unHexLazy :: ToText a => a -> Maybe  LBS.ByteString
unHexLazy v = convertText (toText v) <&> unBase16

unHexStrict ::  ToText a => a -> Maybe  ByteString
unHexStrict v = convertText (toText v) <&> unBase16

unHex :: (Functor f, FromText (f (Base16 b)), ToText a) => a -> f b
unHex v = convertText (toText v) <&> unBase16

forceJust (Just v) =v
forceRight(Right r) = r

