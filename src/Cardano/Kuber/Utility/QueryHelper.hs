{-# LANGUAGE LambdaCase #-}
module Cardano.Kuber.Utility.QueryHelper where
import Cardano.Api
import Cardano.Kuber.Error
    ( ErrorType(EraMisMatch, NodeQueryError, TxSubmissionError, ConnectionError),
      FrameworkError(FrameworkError) )
import Cardano.Api.Shelley (ProtocolParameters, TxBody (ShelleyTxBody), LedgerProtocolParameters (LedgerProtocolParameters))
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
import Cardano.Kuber.Data.Parsers (parseAnyScript)
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Api.Ledger (StandardCrypto)


performShelleyQuery :: LocalNodeConnectInfo CardanoMode -> QueryInShelleyBasedEra ConwayEra b -> IO (Either FrameworkError b)
performShelleyQuery conn q=
  do
  resultE <- perfomEraIndependentQuery conn qFilter
  pure $ resultE >>= \case
        Left em ->  Left $ FrameworkError EraMisMatch  (show em)
        Right res -> pure res

  where
  qFilter = QueryInEra ConwayEraInCardanoMode
                    $ QueryInShelleyBasedEra ShelleyBasedEraConway  q

perfomEraIndependentQuery conn q = do
   a <- queryNodeLocalState conn Nothing q
   case a of
        Left af -> pure $ Left $ FrameworkError NodeQueryError (show q ++ ": Acqure Failure")
        Right result -> pure $ pure $   result


queryUtxos :: LocalNodeConnectInfo CardanoMode-> Set AddressAny -> IO (Either FrameworkError  (UTxO ConwayEra))
queryUtxos conn addr= performShelleyQuery conn (QueryUTxO (QueryUTxOByAddress  addr))

queryAddressInEraUtxos :: LocalNodeConnectInfo CardanoMode -> [AddressInEra ConwayEra ] -> IO (Either FrameworkError  (UTxO ConwayEra))
queryAddressInEraUtxos  conn addrs = performShelleyQuery conn (QueryUTxO (QueryUTxOByAddress $  Set.fromList (map addressInEraToAddressAny  addrs)))

queryTxins :: LocalNodeConnectInfo CardanoMode -> Set TxIn -> IO (Either FrameworkError (UTxO ConwayEra))
queryTxins conn ins= performShelleyQuery conn (QueryUTxO ( QueryUTxOByTxIn ins))

queryGenesesisParams :: LocalNodeConnectInfo CardanoMode-> IO (Either FrameworkError (GenesisParameters ShelleyEra))
queryGenesesisParams con = performShelleyQuery con QueryGenesisParameters

queryChainPoint ::LocalNodeConnectInfo CardanoMode -> IO(Either FrameworkError ChainPoint)
queryChainPoint conn = perfomEraIndependentQuery  conn (QueryChainPoint CardanoMode)

queryProtocolParam :: LocalNodeConnectInfo CardanoMode -> IO (Either FrameworkError (LedgerProtocolParameters ConwayEra))
queryProtocolParam conn= do
  v <- performShelleyQuery conn QueryProtocolParameters
  pure $ case v of
    Left fe -> Left fe
    Right pp -> Right $    LedgerProtocolParameters pp


querySystemStart :: LocalNodeConnectInfo mode -> IO ( Either FrameworkError SystemStart)
querySystemStart conn= perfomEraIndependentQuery conn QuerySystemStart

queryEraHistory :: LocalNodeConnectInfo CardanoMode -> IO ( Either FrameworkError (EraHistory CardanoMode))
queryEraHistory conn=perfomEraIndependentQuery conn  (QueryEraHistory CardanoModeIsMultiEra)


submitTx :: LocalNodeConnectInfo CardanoMode -> Tx ConwayEra -> IO  (Either FrameworkError ())
submitTx conn  tx= do
      res <-submitTxToNodeLocal conn $  TxInMode tx ConwayEraInCardanoMode
      case res of
        SubmitSuccess ->  pure $ pure ()
        SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _eraInMode ->  pure $ Left  $ FrameworkError TxSubmissionError  (show  err)
            TxValidationEraMismatch mismatchErr -> pure $ Left $ FrameworkError TxSubmissionError ("Era Mismatch : " ++ show mismatchErr)
