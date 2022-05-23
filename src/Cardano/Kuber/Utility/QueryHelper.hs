module Cardano.Kuber.Utility.QueryHelper where
import Cardano.Api
import Cardano.Kuber.Error
    ( ErrorType(EraMisMatch, NodeQueryError),
      FrameworkError(FrameworkError) )
import Cardano.Api.Shelley (ProtocolParameters, TxBody (ShelleyTxBody))
import Cardano.Slotting.Time (SystemStart)
import qualified Data.Set as Set
import Data.Set (Set)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess))
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(SubmitFail))
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import Cardano.Kuber.Utility.DataTransformation ( addressInEraToAddressAny )


performQuery :: LocalNodeConnectInfo CardanoMode -> QueryInShelleyBasedEra AlonzoEra b -> IO (Either FrameworkError b)
performQuery conn q=
  do
  a <-queryNodeLocalState conn Nothing  qFilter
  case a of
    Left af -> pure $ Left $ FrameworkError NodeQueryError (show af)
    Right e -> case e of
      Left em -> pure  $ Left $ FrameworkError EraMisMatch  (show em)
      Right uto -> pure $ Right  uto

  where
  qFilter = QueryInEra AlonzoEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo q


queryUtxos :: LocalNodeConnectInfo CardanoMode-> Set AddressAny -> IO (Either FrameworkError  (UTxO AlonzoEra))
queryUtxos conn addr= performQuery conn (QueryUTxO (QueryUTxOByAddress  addr))

queryAddressInEraUtxos :: LocalNodeConnectInfo CardanoMode -> [AddressInEra AlonzoEra ] -> IO (Either FrameworkError  (UTxO AlonzoEra))
queryAddressInEraUtxos  conn addrs = performQuery conn (QueryUTxO (QueryUTxOByAddress $  Set.fromList (map addressInEraToAddressAny  addrs)))

queryTxins :: LocalNodeConnectInfo CardanoMode -> Set TxIn -> IO (Either FrameworkError (UTxO AlonzoEra))
queryTxins conn ins= performQuery conn (QueryUTxO ( QueryUTxOByTxIn ins))


queryProtocolParam :: LocalNodeConnectInfo CardanoMode -> IO ProtocolParameters
queryProtocolParam conn=do
  paramQueryResult<-queryNodeLocalState conn Nothing $
            QueryInEra AlonzoEraInCardanoMode
                  $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
  case paramQueryResult of
    Left af -> error  "QueryProtocolParam: Acquire Failure"
    Right e -> case e of
      Left em -> error "QueryrotocolParam: Missmatched Era"
      Right pp -> return pp

querySystemStart :: LocalNodeConnectInfo mode -> IO SystemStart
querySystemStart conn=do
  result<-queryNodeLocalState conn Nothing QuerySystemStart
  case result of
    Left af -> error "Acquire Failure"
    Right ss -> pure ss

queryEraHistory :: LocalNodeConnectInfo CardanoMode -> IO (EraHistory CardanoMode)
queryEraHistory conn=do
  result <- queryNodeLocalState conn Nothing (QueryEraHistory CardanoModeIsMultiEra)
  case result of
    Left af -> error "Acquire Failure"
    Right eh -> pure eh



signAndSubmitTxBody :: LocalNodeConnectInfo CardanoMode -> TxBody AlonzoEra -> [SigningKey PaymentKey] -> IO (Tx AlonzoEra)
signAndSubmitTxBody conn txBody skeys= do
      let (ins,outs)=case txBody of { ShelleyTxBody sbe (LedgerBody.TxBody ins outs _ _ _ _ _ _ _ _ _ _ _ ) scs tbsd m_ad tsv -> (ins,outs) }
          tx = makeSignedTransaction (map toWitness skeys) txBody -- witness and txBody
      executeSubmitTx conn tx
      pure tx
  where
    toWitness skey = makeShelleyKeyWitness txBody (WitnessPaymentKey skey)

executeSubmitTx :: LocalNodeConnectInfo CardanoMode -> Tx AlonzoEra -> IO ()
executeSubmitTx conn  tx= do
      res <-submitTxToNodeLocal conn $  TxInMode tx AlonzoEraInCardanoMode
      case res of
        SubmitSuccess ->  pure ()
        SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _eraInMode ->  error $ "SubmitTx: " ++ show  err
            TxValidationEraMismatch mismatchErr -> error $ "SubmitTx: " ++ show  mismatchErr
