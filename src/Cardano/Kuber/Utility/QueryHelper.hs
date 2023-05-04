{-# LANGUAGE LambdaCase #-}
module Cardano.Kuber.Utility.QueryHelper where
import Cardano.Api
import Cardano.Kuber.Error
    ( ErrorType(EraMisMatch, NodeQueryError, TxSubmissionError, ConnectionError),
      FrameworkError(FrameworkError) )
import Cardano.Api.Shelley (ProtocolParameters, TxBody (ShelleyTxBody))
import Cardano.Slotting.Time (SystemStart)
import qualified Data.Set as Set
import Data.Set (Set)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess))
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(SubmitFail))
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import Cardano.Kuber.Utility.DataTransformation ( addressInEraToAddressAny )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch(EraMismatch))
import qualified Data.Text as T
import Control.Exception (throw)


performShelleyQuery :: LocalNodeConnectInfo CardanoMode -> QueryInShelleyBasedEra BabbageEra b -> IO (Either FrameworkError b)
performShelleyQuery conn q=
  do
  resultE <- perfomEraIndependentQuery conn qFilter
  pure $ resultE >>= \case
        Left em ->  Left $ FrameworkError EraMisMatch  (show em)
        Right res -> pure res

  where
  qFilter = QueryInEra BabbageEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraBabbage  q

perfomEraIndependentQuery conn q = do 
   a <- queryNodeLocalState conn Nothing q
   case a of
        Left af -> pure $ Left $ FrameworkError NodeQueryError (show q ++ ": Acqure Failure")
        Right result -> pure $ pure $   result


queryUtxos :: LocalNodeConnectInfo CardanoMode-> Set AddressAny -> IO (Either FrameworkError  (UTxO BabbageEra))
queryUtxos conn addr= performShelleyQuery conn (QueryUTxO (QueryUTxOByAddress  addr))

queryAddressInEraUtxos :: LocalNodeConnectInfo CardanoMode -> [AddressInEra BabbageEra ] -> IO (Either FrameworkError  (UTxO BabbageEra))
queryAddressInEraUtxos  conn addrs = performShelleyQuery conn (QueryUTxO (QueryUTxOByAddress $  Set.fromList (map addressInEraToAddressAny  addrs)))

queryTxins :: LocalNodeConnectInfo CardanoMode -> Set TxIn -> IO (Either FrameworkError (UTxO BabbageEra))
queryTxins conn ins= performShelleyQuery conn (QueryUTxO ( QueryUTxOByTxIn ins))

queryGenesesisParams :: LocalNodeConnectInfo CardanoMode-> IO (Either FrameworkError GenesisParameters)
queryGenesesisParams con = performShelleyQuery con QueryGenesisParameters

queryChainPoint ::LocalNodeConnectInfo CardanoMode -> IO(Either FrameworkError ChainPoint)
queryChainPoint conn = perfomEraIndependentQuery  conn (QueryChainPoint CardanoMode)

queryProtocolParam :: LocalNodeConnectInfo CardanoMode -> IO (Either FrameworkError  ProtocolParameters)
queryProtocolParam conn= performShelleyQuery conn QueryProtocolParameters


querySystemStart :: LocalNodeConnectInfo mode -> IO ( Either FrameworkError SystemStart)
querySystemStart conn= perfomEraIndependentQuery conn QuerySystemStart

queryEraHistory :: LocalNodeConnectInfo CardanoMode -> IO ( Either FrameworkError (EraHistory CardanoMode))
queryEraHistory conn=perfomEraIndependentQuery conn  (QueryEraHistory CardanoModeIsMultiEra)


submitTx :: LocalNodeConnectInfo CardanoMode -> Tx BabbageEra -> IO  (Either FrameworkError ())
submitTx conn  tx= do
      res <-submitTxToNodeLocal conn $  TxInMode tx BabbageEraInCardanoMode
      case res of
        SubmitSuccess ->  pure $ pure ()
        SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _eraInMode ->  pure $ Left  $ FrameworkError TxSubmissionError  (show  err)
            TxValidationEraMismatch mismatchErr -> pure $ Left $ FrameworkError TxSubmissionError ("Era Mismatch : " ++ show mismatchErr)
