{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Kuber.Utility.WalletUtil where
import Cardano.Api (SigningKey, PaymentKey, TxBody, AlonzoEra, Tx, makeSignedTransaction, makeShelleyKeyWitness, ShelleyWitnessSigningKey (WitnessPaymentKey))
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import System.Directory ( doesFileExist )
import Cardano.Kuber.Data.Parsers ( parseSignKey )
import Cardano.Kuber.Utility.ChainInfoUtil ( getWorkPath )
import Control.Exception (try)


-- readSignKey relative to CARDANO_HOME path
-- this funciton can throw errors
readSignKey :: FilePath -> IO (SigningKey PaymentKey)
readSignKey file = do
  eitherSkey<-try  readSkeyFromFile
  case eitherSkey of
    Left (e::IOError )-> fail  "There was error reading skey file"
    Right sk -> pure sk
  where
    readSkeyFromFile=do
      exists<-doesFileExist file
      if exists then pure () else  fail $ file ++ "  doesn't exist"
      content <-readBs file
      parseSignKey content

    readBs:: FilePath -> IO T.Text
    readBs  = TextIO.readFile


-- read the default.skey frole from   CARDANO_HOME directory.
-- the file contains the filename of the which is to be read.
getDefaultSignKey :: IO (SigningKey PaymentKey)
getDefaultSignKey= getWorkPath ["default.skey"] >>= readSignKey


signTxBody :: TxBody AlonzoEra -> [SigningKey PaymentKey] -> Tx AlonzoEra
signTxBody txBody skeys= do
          makeSignedTransaction (map toWitness skeys) txBody
  where
    toWitness skey = makeShelleyKeyWitness txBody (WitnessPaymentKey skey) 