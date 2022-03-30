module Core where

import Cardano.Api
import Cardano.Contrib.Easy.Context
import Cardano.Contrib.Easy.Error
import Cardano.Contrib.Easy.Models
  ( BalanceResponse (BalanceResponse),
    PayToModel (PayToModel),
    SubmitTxModal (SubmitTxModal),
    TxResponse (TxResponse),
  )
import Cardano.Contrib.Easy.Parsers (parseValueText)
import Cardano.Contrib.Easy.TxFramework (TxResult (TxResult), mkTx, txPayTo)
import Cardano.Contrib.Easy.Util (executeSubmitTx, queryUtxos)
import Control.Exception (throw)
import qualified Data.Text as T

getBalance :: (IsNetworkCtx v) => v -> String -> IO BalanceResponse
getBalance ctx addrStr = do
  addr <- case deserialiseAddress AsAddressAny $ T.pack addrStr of
    Nothing -> throw $ SomeError "Invalid address"
    Just aany -> pure aany

  utxos <- queryUtxos (networkCtxConn ctx) addr

  pure $ BalanceResponse utxos

submitTx :: IsNetworkCtx v => v -> SubmitTxModal -> IO TxResponse
submitTx ctx (SubmitTxModal tx mWitness) = do
  let tx' = case mWitness of
        Nothing -> tx
        Just kw -> makeSignedTransaction (kw : getTxWitnesses tx) txbody
      txbody = getTxBody tx
  executeSubmitTx (networkCtxConn ctx) tx'
  pure $ TxResponse tx' []

-- buildTx :: IsNetworkCtx v => v -> String -> String -> IO TxResponse
-- buildTx ctx addrStr amountStr = do
--   addr <- case deserialiseAddress AsAddressAny $ T.pack addrStr of
--     Nothing -> throw $ SomeError "Invalid address"
--     Just aany -> pure aany

--   amount <- case readMaybe amountStr of
--     Nothing -> throw $ SomeError "Invalid amount"
--     Just a -> pure a

--   let tx = makeTransaction [makeTxIn (OutPoint (TxId "") 0) maxBound]
--         [makeTxOut addr amount]
--   pure $ TxResponse tx []

payToTx :: IsNetworkCtx v => v -> PayToModel -> IO String
payToTx ctx (PayToModel sender receiver value) = do
  senderAddr <- case deserialiseAddress (AsAddressInEra AsAlonzoEra) (T.pack receiver) of
    Just addr -> pure addr
    Nothing -> fail "Invalid receiver address"

  receiverAddr <- case deserialiseAddress (AsAddressInEra AsAlonzoEra) (T.pack receiver) of
    Just addr -> pure addr
    Nothing -> fail "Invalid receiver address"

  val <- parseValueText $ T.pack value

  let txOperation = txPayTo receiverAddr val
  TxResult _ _ _ txbody <- mkTx ctx txOperation senderAddr
  let txRes = TxResponse (makeSignedTransaction [] txbody) []
  pure $ "ok" ++ show txRes