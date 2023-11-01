{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Kuber.Server.Core where

import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy             as TL
import Cardano.Api
import Control.Exception (throw, try)
import qualified Data.Set as Set
import System.Exit (die)
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import System.Environment (getEnv)
import System.FilePath (joinPath)
import Cardano.Ledger.Alonzo.Scripts (ExUnits(ExUnits))
import Data.Text.Conversions (Base16(Base16), convertText)
import Cardano.Api.Shelley (TxBody(ShelleyTxBody), fromShelleyTxIn)
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Alonzo.TxBody (inputs')
import qualified Data.Map as Map
import Data.Text (Text)
import Cardano.Kuber.Data.Models
import qualified Data.ByteString.Char8 as BS8
import Data.Functor ((<&>))
import Cardano.Kuber.Data.Parsers (parseTxIn, parseAddressBech32, parseAddressBech32)
import qualified Debug.Trace as Debug
import Data.Word (Word64)
import qualified Data.Aeson.Key as A
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.Aeson ((.:), ToJSON (toJSON))
import Kuber.Server.Model
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Encoding as A


makeHandler a  kontract = liftIO $ evaluateKontract a kontract  >>= (\case
   Left fe -> throw fe
   Right pp -> pure pp)

makeHandler1 a  f p1 = makeHandler a (f p1)
makeHandler2 a  f p1 p2= makeHandler a (f p1 p2)


queryUtxosHandler :: HasChainQueryAPI api =>   [Text] ->  [Text] -> Kontract api w FrameworkError UtxoModal
queryUtxosHandler   [] [] = KError (FrameworkError ParserError "Missing both address and txin in query param")
queryUtxosHandler   addrTxts txinTxts = do
      if null addrTxts
        then do
          txins <- mapM (\v1 -> case parseTxIn  v1  of
                Right v -> pure v
                Left msg -> KError (FrameworkError ParserError msg)
            ) txinTxts
          kQueryUtxoByTxin (Set.fromList txins) <&> UtxoModal
      else if null txinTxts
        then do
          addrs <-  mapM (\v1 -> case parseAddressBech32  v1 of
                    Right v -> pure  $ addressInEraToAddressAny v
                    Left msg -> KError (FrameworkError ParserError msg)
                ) addrTxts
          kQueryUtxoByAddress (Set.fromList addrs) <&> UtxoModal
      else
        KError (FrameworkError ParserError "Expected either address or txin in parameter")


getKeyHashHandler :: AddressModal -> IO KeyHashResponse
getKeyHashHandler aie = do
  case addressInEraToPaymentKeyHash (unAddressModal aie) of
    Nothing -> throw $ FrameworkError  ParserError  "Couldn't derive key-hash from address "
    Just ha -> pure $ KeyHashResponse $ BS8.unpack $ serialiseToRawBytesHex ha

translatePosixTimeHandler :: HasKuberAPI a =>  TimeTranslationReq -> Kontract a w FrameworkError TranslationResponse
translatePosixTimeHandler (TimeTranslationReq timestamp) = do
    TranslationResponse <$> kTimeToSlot timestamp <*> pure timestamp

translateSlotHandler :: HasKuberAPI a => SlotTranslationReq -> Kontract a w FrameworkError TranslationResponse
translateSlotHandler (SlotTranslationReq slotNo) = do
    TranslationResponse  slotNo <$> kSlotToTime slotNo

queryTimeHandler :: HasKuberAPI a =>  Kontract a w FrameworkError TranslationResponse
queryTimeHandler  = do
    now <- liftIO getPOSIXTime
    translatePosixTimeHandler (TimeTranslationReq now)


queryBalanceHandler :: HasChainQueryAPI a => Text ->   Kontract a w FrameworkError BalanceResponse
queryBalanceHandler addrStr =
    case parseAddressBech32 addrStr of
      Left e -> KError $ FrameworkError ParserError e
      Right a -> do
        utxos<- kQueryUtxoByAddress (Set.singleton $ addressInEraToAddressAny  a)
        pure $ BalanceResponse utxos

txBuilderHandler :: HasKuberAPI a => Maybe Bool -> TxBuilder -> Kontract a w FrameworkError TxModal
txBuilderHandler submitM txBuilder = do
  liftIO $ putStrLn $ BS8.unpack $  prettyPrintJSON txBuilder
  case submitM of 
    Just True ->  kBuildAndSubmit txBuilder <&> TxModal
    _ ->  kBuildTx txBuilder <&> TxModal
 

submitTxHandler :: HasSubmitApi a =>  SubmitTxModal -> Kontract a w FrameworkError TxModal
submitTxHandler  (SubmitTxModal tx mWitness) = do
  let tx' = case mWitness of
        Nothing -> tx
        Just kw -> makeSignedTransaction (kw : getTxWitnesses tx) txbody
      txbody = getTxBody tx
  kSubmitTx tx'
  pure $ TxModal tx'