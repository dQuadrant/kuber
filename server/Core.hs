{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Core where

import qualified Data.Text as T
import Cardano.Contrib.Kuber.TxBuilder (TxBuilder)
import qualified Data.Aeson as A
import Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy             as TL
import Cardano.Contrib.Kuber.Models (BalanceResponse (BalanceResponse), TxResponse (TxResponse), SubmitTxModal (SubmitTxModal), TxModal (TxModal))
import Cardano.Contrib.Kuber.ChainInfo (ChainConnectInfo, ChainInfo (getConnectInfo), DetailedChainInfo (DetailedChainInfo))
import Cardano.Contrib.Kuber.Util (queryUtxos, executeSubmitTx, readSignKey, queryTxins, resolveTxins)
import Cardano.Api
import Control.Exception (throw, try)
import Cardano.Contrib.Kuber.Error (FrameworkError(FrameworkError), ErrorType (ParserError, LibraryError, ExUnitCalculationError))
import qualified Data.Set as Set
import System.Exit (die)
import Cardano.Contrib.Kuber.TxFramework (txBuilderToTxBody)
import System.Environment (getEnv)
import System.FilePath (joinPath)
import Cardano.Ledger.Alonzo.Scripts (ExUnits(ExUnits))
import Data.Text.Conversions (Base16(Base16), convertText)
import Cardano.Api.Shelley (TxBody(ShelleyTxBody), fromShelleyTxIn)
import Cardano.Ledger.Shelley.API (TxBody(_inputs))
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Alonzo.TxBody (inputs')
import qualified Data.Map as Map
import Data.Text (Text)



getBalance :: ChainConnectInfo -> String -> IO BalanceResponse
getBalance ctx addrStr = do
  addr <- case deserialiseAddress AsAddressAny $ T.pack addrStr of
    Nothing -> throw $ FrameworkError  ParserError  "Invalid address"
    Just aany -> pure aany
  eUtxos <- queryUtxos (getConnectInfo ctx) $ Set.singleton addr
  case eUtxos of 
    Left fe -> throw fe
    Right utxos -> pure $ BalanceResponse  utxos

submitTx :: ChainConnectInfo -> SubmitTxModal -> IO TxResponse
submitTx ctx (SubmitTxModal tx mWitness) = do
  let tx' = case mWitness of
        Nothing -> tx
        Just kw -> makeSignedTransaction (kw : getTxWitnesses tx) txbody
      txbody = getTxBody tx
  executeSubmitTx (getConnectInfo ctx) tx'
  pure $ TxResponse tx'

txBuilder :: DetailedChainInfo  ->  TxBuilder -> IO TxResponse
txBuilder dcinfo txBuilder = do
  let encodedTxBuilder = A.encode txBuilder
  let txBuilderStr = TL.unpack $ TL.decodeUtf8 encodedTxBuilder
  print txBuilderStr
  txBodyE<-txBuilderToTxBody dcinfo txBuilder
  case txBodyE of 
    Left fe -> throw fe
    Right txBody -> pure $ TxResponse $ makeSignedTransaction [] txBody

testTx :: DetailedChainInfo  ->  TxModal -> IO TxResponse
testTx dcinfo txBuilder = do
    fail "sad"
  -- let encodedTxBuilder = A.encode txBuilder
  -- let txBuilderStr = TL.unpack $ TL.decodeUtf8 encodedTxBuilder
  -- print txBuilderStr
  -- txBodyE<-txBuilderToTxBody dcinfo txBuilder
  -- case txBodyE of 
  --   Left fe -> throw fe
  --   Right txBody -> do
  --     signKey <- getDefaultSignKey
  --     print txBody
  --     let keyWit = makeShelleyKeyWitness txBody (WitnessPaymentKey signKey)
  --         tx = makeSignedTransaction [keyWit] txBody
  --     executeSubmitTx (getConnectInfo dcinfo) tx
  --     pure $ TxResponse tx


evaluateExecutionUnits' :: DetailedChainInfo ->  Tx AlonzoEra -> IO [Either String ExecutionUnits]
evaluateExecutionUnits' (DetailedChainInfo costPerWord conn pParam systemStart eraHistory)  tx = do 
      let txbody =  getTxBody tx 

          inputs :: Set.Set TxIn 
          inputs = case txbody of {ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn   $ inputs' tb }
      txins <- resolveTxins  conn  inputs 
      case txins of 
        Left fe -> throw $ FrameworkError  ExUnitCalculationError  (show fe)
        Right uto -> case evaluateTransactionExecutionUnits AlonzoEraInCardanoMode systemStart eraHistory pParam uto txbody of 
            Left tve -> throw $ FrameworkError ExUnitCalculationError $ show tve
            Right exUnitMap -> pure $ Prelude.map (\case
                                        Left see -> Left (show see)
                                        Right eu -> Right eu  )   (Map.elems exUnitMap)

evaluateExecutionUnits :: DetailedChainInfo ->  String -> IO [Either String ExecutionUnits]
evaluateExecutionUnits dcinfo  txStr = do 
      case convertText txStr of
        Nothing -> fail "Tx string is not hex encoded"
        Just (Base16 bs) -> case deserialiseFromCBOR (AsTx AsAlonzoEra ) bs of
          Left  e -> fail $ "Tx string: Invalid CBOR format : "++ show e
          Right tx -> evaluateExecutionUnits' dcinfo  tx

getDefaultSignKey :: IO (SigningKey PaymentKey)
getDefaultSignKey= getWorkPath ["default.skey"] >>= readSignKey

getWorkPath :: [FilePath] -> IO  FilePath
getWorkPath paths= do
  eitherHome <-try $ getEnv "HOME"
  case eitherHome of
    Left (e::IOError) -> throw $ FrameworkError LibraryError "Can't get Home directory"
    Right home -> do
      pure $ joinPath  $  [home , ".cardano"] ++ paths