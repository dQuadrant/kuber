{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Kuber.Utility.QueryHelper where
import Cardano.Api hiding (queryCurrentEra)
import Cardano.Kuber.Error
    ( ErrorType(EraMisMatch, NodeQueryError, TxSubmissionError, ConnectionError, FeatureNotSupported),
      FrameworkError(FrameworkError) )
import Cardano.Api.Shelley (ProtocolParameters, TxBody (ShelleyTxBody), LedgerProtocolParameters (LedgerProtocolParameters), ShelleyLedgerEra)
import Cardano.Slotting.Time (SystemStart)
import qualified Data.Set as Set
import Data.Set (Set)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(SubmitSuccess))
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(SubmitFail))
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import Cardano.Kuber.Utility.DataTransformation ( addressInEraToAddressAny )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch(EraMismatch))
import qualified Data.Text as T
import Control.Exception (throw, catch, SomeException (SomeException), IOException)
import Cardano.Kuber.Data.Parsers (parseAnyScript)
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Api.Ledger (StandardCrypto, Credential, KeyRole (DRepRole), DRepState)
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra)
import GHC.IO.Exception (IOException(..), IOErrorType (..))
import Data.Map (Map)


performShelleyQuery :: IsShelleyBasedEra era => LocalNodeConnectInfo CardanoMode -> QueryInShelleyBasedEra era b -> String -> IO (Either FrameworkError b)
performShelleyQuery =performShelleyQuery' shelleyBasedEra

performShelleyQuery' :: ShelleyBasedEra era ->LocalNodeConnectInfo CardanoMode -> QueryInShelleyBasedEra era b -> String -> IO (Either FrameworkError b)
performShelleyQuery' sbera conn q queryName=
  do
  resultE <- perfomEraIndependentQuery conn qFilter queryName
  pure $ resultE >>= \case
        Left em ->  Left $ FrameworkError EraMisMatch  (case em of { EraMismatch txt txt' -> "When performing " ++ queryName ++" query, Kuber is on" ++ T.unpack  txt ++" era but node is at "++ T.unpack txt' } )
        Right res -> pure res
  where
  qFilter = QueryInEra (getErainMode  sbera)
                    $ QueryInShelleyBasedEra sbera  q
perfomEraIndependentQuery conn q queryName = do
  catch (catch ( do
      a <- queryNodeLocalState conn Nothing q
      case a of
            Left af -> pure $ Left $ FrameworkError NodeQueryError (show q ++ ": Acqure Failure")
            Right result -> pure $ pure result
    ) (\(e ::IOException )-> pure $ Left $ FrameworkError  ConnectionError (case e of {
      IOError m_han iet s str m_ci m_s -> "Query" ++ queryName ++ ": " ++ case iet of
        ResourceBusy -> "ResourceBusy: path=" ++  filePath
        EOF -> "EOF on while reading: path=" ++  filePath
        IllegalOperation -> "Illegal connection attempt: path=" ++  filePath
        PermissionDenied -> "Permission denied: path=" ++  filePath
        TimeExpired ->  "TimeOut on connection: path=" ++  filePath
        ResourceVanished -> "Resource vanished while reading: path=" ++  filePath
        Interrupted -> "Interrupted during connection"
        NoSuchThing -> "File not found: path=" ++ filePath
        _          ->  "IOError :" ++ show e
            } ))
    ) (\(e :: SomeException)-> pure $ Left $ FrameworkError  ConnectionError ( "Query" ++ queryName ++ ": " ++ show e) )
  where
    filePath = case conn of { LocalNodeConnectInfo cmp ni fi -> case fi of { File s -> s }  }
    -- evaluateMessage msg = if "No such file or directory" `isInfixOf` msg
    --                         then case conn of
    --                         else ""
queryUtxos :: IsShelleyBasedEra era => LocalNodeConnectInfo CardanoMode-> Set AddressAny -> IO (Either FrameworkError  (UTxO era))
queryUtxos conn addr= performShelleyQuery conn (QueryUTxO (QueryUTxOByAddress  addr)) "Utxo"

queryAddressInEraUtxos :: IsShelleyBasedEra era => LocalNodeConnectInfo CardanoMode -> [AddressInEra ConwayEra ] -> IO (Either FrameworkError  (UTxO era))
queryAddressInEraUtxos  conn addrs = performShelleyQuery conn (QueryUTxO (QueryUTxOByAddress $  Set.fromList (map addressInEraToAddressAny  addrs))) "Utxo"

queryTxins :: IsShelleyBasedEra era => LocalNodeConnectInfo CardanoMode -> Set TxIn -> IO (Either FrameworkError (UTxO era))
queryTxins conn ins= performShelleyQuery conn (QueryUTxO ( QueryUTxOByTxIn ins)) "TxInput"


queryGenesesisParams' ::  LocalNodeConnectInfo CardanoMode-> IO (Either FrameworkError (GenesisParameters ShelleyEra))
queryGenesesisParams'  con = do
  era <- queryCurrentEra con
  case era of
    Left fe -> pure $ Left fe
    Right ace -> case ace of { AnyCardanoEra ce -> case ce of
                                 ByronEra ->  pure $ Left $ FrameworkError FeatureNotSupported "Query Genesis Parameter is not supported in ByronEra"
                                 ShelleyEra -> performShelleyQuery @ShelleyEra con QueryGenesisParameters "GenesisParam"
                                 AllegraEra -> performShelleyQuery @AllegraEra con QueryGenesisParameters "GenesisParam"
                                 MaryEra -> performShelleyQuery @MaryEra con QueryGenesisParameters "GenesisParam"
                                 AlonzoEra -> performShelleyQuery @AllegraEra con QueryGenesisParameters "GenesisParam"
                                 BabbageEra -> performShelleyQuery @BabbageEra con QueryGenesisParameters "GenesisParam"
                                 ConwayEra -> performShelleyQuery @ConwayEra con QueryGenesisParameters "GenesisParam"  }

queryGenesesisParams :: ShelleyBasedEra era -> LocalNodeConnectInfo CardanoMode-> IO (Either FrameworkError (GenesisParameters ShelleyEra))
queryGenesesisParams sbera con = performShelleyQuery' sbera  con QueryGenesisParameters "GenesisParam"

queryChainPoint ::LocalNodeConnectInfo CardanoMode -> IO(Either FrameworkError ChainPoint)
queryChainPoint conn = perfomEraIndependentQuery  conn (QueryChainPoint CardanoMode) "ChainPoint"


queryProtocolParam :: IsShelleyBasedEra era => LocalNodeConnectInfo CardanoMode -> IO (Either FrameworkError (LedgerProtocolParameters era))
queryProtocolParam conn= do
  v <- performShelleyQuery conn QueryProtocolParameters "ProtocolParameters"
  pure $ case v of
    Left fe -> Left fe
    Right pp -> Right $    LedgerProtocolParameters pp


querySystemStart :: LocalNodeConnectInfo mode -> IO ( Either FrameworkError SystemStart)
querySystemStart conn= perfomEraIndependentQuery conn QuerySystemStart "SystemStart"

queryCurrentEra :: LocalNodeConnectInfo CardanoMode -> IO (Either FrameworkError AnyCardanoEra)
queryCurrentEra conn= do perfomEraIndependentQuery conn (QueryCurrentEra CardanoModeIsMultiEra) "CurrentEra"

queryEraHistory :: LocalNodeConnectInfo CardanoMode -> IO ( Either FrameworkError (EraHistory CardanoMode))
queryEraHistory conn=perfomEraIndependentQuery conn  (QueryEraHistory CardanoModeIsMultiEra) "EraHistory"

queryStakeDeposits ::   ShelleyBasedEra era -> LocalNodeConnectInfo CardanoMode -> Set StakeCredential -> IO      (Either   FrameworkError (Map StakeCredential Lovelace))
queryStakeDeposits  era conn creds =performShelleyQuery @ConwayEra conn (QueryStakeDelegDeposits creds) "StakeDelegDeposits"

queryConstitution :: IsShelleyBasedEra era =>  LocalNodeConnectInfo CardanoMode -> IO      (Either         FrameworkError         (Maybe   (Ledger.Constitution  (ShelleyLedgerEra  era))))
queryConstitution  conn  =performShelleyQuery  conn QueryConstitution "Constitution"

queryGovState :: IsShelleyBasedEra era => LocalNodeConnectInfo CardanoMode -> IO      (Either FrameworkError (Ledger.GovState (ShelleyLedgerEra era)))
queryGovState  conn  =performShelleyQuery   conn QueryGovState "GovState"

queryDRepState :: ShelleyBasedEra era -> LocalNodeConnectInfo CardanoMode -> Set (Credential 'DRepRole StandardCrypto) -> IO      (Either  FrameworkError   (Map   (Credential   'DRepRole StandardCrypto)  (DRepState StandardCrypto)))
queryDRepState  era conn drep =performShelleyQuery' era  conn (QueryDRepState drep ) "DrepState"

submitTx :: LocalNodeConnectInfo CardanoMode -> InAnyCardanoEra Tx -> IO  (Either FrameworkError ())
submitTx conn  (InAnyCardanoEra era tx)= do
      res <-submitTxToNodeLocal conn $  TxInMode tx (getErainMode' era)
      case res of
        SubmitSuccess ->  pure $ pure ()
        SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _eraInMode ->  pure $ Left  $ FrameworkError TxSubmissionError  (show  err)
            TxValidationEraMismatch mismatchErr -> pure $ Left $ FrameworkError TxSubmissionError ("Era Mismatch : " ++ show mismatchErr)


getErainMode :: ShelleyBasedEra era -> EraInMode era CardanoMode
getErainMode  era = case era of
  ShelleyBasedEraShelley -> ShelleyEraInCardanoMode
  ShelleyBasedEraAllegra -> AllegraEraInCardanoMode
  ShelleyBasedEraMary -> MaryEraInCardanoMode
  ShelleyBasedEraAlonzo -> AlonzoEraInCardanoMode
  ShelleyBasedEraBabbage -> BabbageEraInCardanoMode
  ShelleyBasedEraConway -> ConwayEraInCardanoMode

getErainMode' :: CardanoEra era -> EraInMode era CardanoMode
getErainMode'  era = case era of
  ShelleyEra -> ShelleyEraInCardanoMode
  AllegraEra -> AllegraEraInCardanoMode
  MaryEra -> MaryEraInCardanoMode
  AlonzoEra -> AlonzoEraInCardanoMode
  BabbageEra -> BabbageEraInCardanoMode
  ConwayEra -> ConwayEraInCardanoMode
  ByronEra -> ByronEraInCardanoMode