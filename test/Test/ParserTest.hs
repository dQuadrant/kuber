{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.ParserTest where

import qualified Data.Text as T
import Cardano.Api hiding (parseAssetId)
import Cardano.Kuber.Util hiding (toHexString)
import Data.Text.Conversions
import Data.ByteString (ByteString)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Data.Functor ((<&>))
import Test.Tasty.HUnit (testCase, (@?=))
import Cardano.Kuber.Data.Parsers
import qualified Debug.Trace as Debug
import qualified Data.ByteString as BS
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable(toConsoleText, toConsoleTextNoPrefix))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Cardano.Ledger.Shelley.API (ScriptHash(ScriptHash))
import PlutusLedgerApi.V1 (PubKeyHash(..), fromBuiltin)
import Cardano.Kuber.Data.Models (submitTxModalValue)
import Control.Applicative ((<|>))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Utils.addrInEraToPkh" [
      baseAddressToPkh
  ,  parseutxoWithOnlyLovelace
  ,  parseUtxoWithAsset
  , parseAssetIdTest
  , submitTxModalJsonShape

  ]


tAddressToPkh  :: String ->  Maybe String
tAddressToPkh address = deserialiseAddress (AsAddressInEra AsConwayEra) (T.pack address) >>= addrInEraToPkh <&> (\(PubKeyHash pkh ) -> toHexString $ fromBuiltin pkh )


baseAddressToPkh :: TestTree
baseAddressToPkh = testCase "should parse full shelley base address" (tAddressToPkh  "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj"
                                                            @?= Just "f735cdaa2ae360ce67e27cd2f6038a44c77bf3ede94bd144674d34e6")

parseutxoWithOnlyLovelace:: TestTree
parseutxoWithOnlyLovelace=testCase "should parse utxo with only lovelace output" $
  (parseUtxo  @BabbageEra (T.pack "828258208d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c05420282583901538d169e31bec9d9903f53cc2fce8fffc6d61b30298976bb20f48970f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b1a02addd3a")
    <&> toConsoleTextNoPrefix ) @?= Just "8d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c0542#2 : 44.948794 Ada"

parseUtxoWithAsset :: TestTree
parseUtxoWithAsset =  testCase  "should parse utxo with asset" $
      (parseUtxo @BabbageEra  (T.pack  "828258208d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c05420682583901538d169e31bec9d9903f53cc2fce8fffc6d61b30298976bb20f48970f9c9e87246d2f0373885896ad2804b7229673204cac9208345c1ea5b821a002c3268a1581c4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36a14c466c6f77657279566964656f01")
        <&> toConsoleTextNoPrefix ) @?= Just "8d3921f63a5d65c337cf1f462a086e39ffef68a0b0a1576a9ead2525903c0542#6 : 2.896488 Ada +1 4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36.FloweryVideo"

parseAssetIdTest :: TestTree
parseAssetIdTest = testCase " should parse with dot"  (parseAssetId (T.pack " 4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36.Flowery. Video\n ")
   @?= Just (AssetId  ( forceRight $  (deserialiseFromRawBytesHex (BS8.pack "4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36") :: Either RawBytesHexError PolicyId)) (forceRight $ deserialiseFromRawBytes AsAssetName (BS8.pack "Flowery. Video")))
  )

parseAssetIdHex :: TestTree
parseAssetIdHex = testCase " should parse assetId Hex"  (parseAssetId (T.pack " 4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d3604f59d3d36\n ")
   @?= Just (AssetId  ( forceRight $  (deserialiseFromRawBytesHex (BS8.pack "4b36a781645ef8eea2a75687edc16b2d0aa4be3016eeed04f59d3d36") :: Either RawBytesHexError PolicyId)) ( forceRight $  (deserialiseFromRawBytesHex (BS8.pack "04f59d3d36") :: Either RawBytesHexError AssetName)))
  )

submitTxModalJsonShape :: TestTree
submitTxModalJsonShape = testCase "SubmitTxModal encodes using the tx wrapper field" $ do
  let encoded = submitTxModalValue (TextEnvelope "Tx ConwayEra" "" "deadbeef") Nothing
  case encoded of
    A.Object obj -> do
      KM.lookup "tx" obj /= Nothing @?= True
      KM.lookup "cborHex" obj /= Nothing @?= True
      case KM.lookup "tx" obj of
        Just (A.Object txObj) -> KM.lookup "cborHex" txObj /= Nothing @?= True
        _ -> error "Expected tx field to contain an object"
    _ -> error "Expected SubmitTxModal to encode as an object"


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
