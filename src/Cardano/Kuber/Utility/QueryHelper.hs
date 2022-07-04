module Cardano.Kuber.Utility.QueryHelper where
import Cardano.Api
import Cardano.Kuber.Error
    ( ErrorType(EraMisMatch, NodeQueryError, TxSubmissionError),
      FrameworkError(FrameworkError) )
import Cardano.Api.Shelley (ProtocolParameters, TxBody (ShelleyTxBody))
import Cardano.Slotting.Time (SystemStart)
import qualified Data.Set as Set
import Data.Set (Set)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess))
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(SubmitFail))
import qualified Cardano.Ledger.Babbage.TxBody as LedgerBody
import Cardano.Kuber.Utility.DataTransformation ( addressInEraToAddressAny )


performQuery :: LocalNodeConnectInfo CardanoMode -> QueryInShelleyBasedEra BabbageEra b -> IO (Either FrameworkError b)
performQuery conn q=
  do
  a <-queryNodeLocalState conn Nothing  qFilter
  case a of
    Left af -> pure $ Left $ FrameworkError NodeQueryError (show af)
    Right e -> case e of
      Left em -> pure  $ Left $ FrameworkError EraMisMatch  (show em)
      Right uto -> pure $ Right  uto

  where
  qFilter = QueryInEra BabbageEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraBabbage  q


queryUtxos :: LocalNodeConnectInfo CardanoMode-> Set AddressAny -> IO (Either FrameworkError  (UTxO BabbageEra))
queryUtxos conn addr= performQuery conn (QueryUTxO (QueryUTxOByAddress  addr))

queryAddressInEraUtxos :: LocalNodeConnectInfo CardanoMode -> [AddressInEra BabbageEra ] -> IO (Either FrameworkError  (UTxO BabbageEra))
queryAddressInEraUtxos  conn addrs = performQuery conn (QueryUTxO (QueryUTxOByAddress $  Set.fromList (map addressInEraToAddressAny  addrs)))

queryTxins :: LocalNodeConnectInfo CardanoMode -> Set TxIn -> IO (Either FrameworkError (UTxO BabbageEra))
queryTxins conn ins= performQuery conn (QueryUTxO ( QueryUTxOByTxIn ins))



queryProtocolParam :: LocalNodeConnectInfo CardanoMode -> IO ProtocolParameters
queryProtocolParam conn=do
  paramQueryResult<-queryNodeLocalState conn Nothing $
            QueryInEra BabbageEraInCardanoMode
                  $ QueryInShelleyBasedEra ShelleyBasedEraBabbage QueryProtocolParameters
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


submitTx :: LocalNodeConnectInfo CardanoMode -> Tx BabbageEra -> IO  (Either FrameworkError ())
submitTx conn  tx= do
      res <-submitTxToNodeLocal conn $  TxInMode tx BabbageEraInCardanoMode
      case res of
        SubmitSuccess ->  pure $ pure ()
        SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _eraInMode ->  pure $ Left  $ FrameworkError TxSubmissionError  (show  err)
            TxValidationEraMismatch mismatchErr -> pure $ Left $ FrameworkError TxSubmissionError ("Era Mismatch : " ++ show mismatchErr)
