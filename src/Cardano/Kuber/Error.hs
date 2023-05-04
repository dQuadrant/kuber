{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.Kuber.Error
where

import GHC.Exception.Type (Exception)
import Data.Aeson (object, ToJSON (toJSON), KeyValue ((.=)), FromJSON (parseJSON))
import Control.Exception (throw)
import Cardano.Api
import Plutus.ApiCommon (EvaluationError(..))
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS8
import Cardano.Api.Shelley
import Cardano.Ledger.Babbage.Tx (ValidatedTx(ValidatedTx), ScriptPurpose)
import Cardano.Ledger.Shelley.LedgerState (witsFromTxWitnesses)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Hashes as Ledger
import Cardano.Kuber.Utility.Text (toHexString)
import Data.List (intercalate)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as A

data ErrorType =  ConnectionError
                | BalancingError
                | InsufficientInput
                | EraMisMatch
                | NodeQueryError
                | LibraryError
                | ParserError
                | PlutusScriptError -- Plutus Script was executed and it returned error
                | PlutusExecutionError -- There was error executing the plutus script . We couldn't execute the script.
                | ExUnitCalculationError
                | FeatureNotSupported
                | TxValidationError
                | BadMetadata
                | TxSubmissionError
                | WrongScriptType deriving Show



data FrameworkError =  FrameworkError{
                    feType:: ErrorType,
                    feMessage :: String
                  }
        | FrameworkErrors [FrameworkError]

instance FromJSON FrameworkError where
  parseJSON (A.Object o) = do
      eTypeStr  <- o A..:  "type"
      etype <- case eTypeStr ::String of
              "ConnectionError" ->  pure ConnectionError
              "BalancingError" ->  pure BalancingError
              "InsufficientInput" -> pure InsufficientInput
              "EraMisMatch" -> pure EraMisMatch
              "NodeQueryError" -> pure NodeQueryError
              "LibraryError" -> pure LibraryError
              "ParserError" -> pure ParserError
              "PlutusScriptError" -> pure PlutusScriptError
              "PlutusExecutionError" -> pure PlutusExecutionError
              "ExUnitCalculationError" -> pure ExUnitCalculationError
              "FeatureNotSupported" -> pure FeatureNotSupported
              "TxValidationError" -> pure TxValidationError
              "BadMetadata" -> pure BadMetadata
              "TxSubmissionError" -> pure TxSubmissionError
              "WrongScriptType" -> pure WrongScriptType
              _ -> fail "Invalid error type"
      message <- o A..: "message"
      pure $ FrameworkError etype message
  parseJSON  _ = fail "Expected FrameworkError object"


instance Show FrameworkError where
  show  (FrameworkError t m)= "FrameworkError: "++show t ++ ": "++show m

instance ToJSON FrameworkError where
  toJSON (FrameworkError t m) = object ["type" .= show t, "message" .= m]
  toJSON (FrameworkErrors errs) = object [ "messages" .= errs]





instance Exception FrameworkError


throwFrameworkError :: Applicative m => Either FrameworkError a-> m a
throwFrameworkError = \case
    Left e -> throw e
    Right v -> pure v




fromScriptExecutionError :: ScriptExecutionError -> TxBody BabbageEra -> FrameworkError
fromScriptExecutionError see  txbody=  case see of
                ScriptErrorMissingTxIn ti -> makeErr  ("Input Missing : " ++ T.unpack (renderTxIn ti))
                ScriptErrorTxInWithoutDatum ti -> makeErr  ("Input doesn't have datum " ++  T.unpack (renderTxIn ti))
                ScriptErrorWrongDatum ha ->  makeErr  ("Worng datum provided for hash " ++  BS8.unpack (serialiseToRawBytesHex ha))
                ScriptErrorEvaluationFailed ee txts -> case ee of
                  CekError ewc -> mkPlutusErr ("CekError : " ++ show ewc ++ " : " ++ show txts)
                  DeBruijnError fve -> mkPlutusErr ("DeBruijnError : " ++ show fve ++ " : " ++ show txts)
                  CodecError df -> mkPlutusErr ("CodecError Deserialization : " ++ show df ++ " : " ++ show txts)
                  IncompatibleVersionError ver -> mkPlutusErr ("Incompatible version  : " ++ show ver)
                  CostModelParameterMismatch -> mkPlutusErr "Unexpected costModel Parameter Mismatch"
                ScriptErrorExecutionUnitsOverflow -> makeErr "Execution Units Overflowed "
                ScriptErrorNotPlutusWitnessedTxIn swi sh -> makeErr $ "Trying to execute non-plutus script : " ++  BS8.unpack (serialiseToRawBytesHex sh)
                ScriptErrorRedeemerPointsToUnknownScriptHash swi -> makeErr $ "Unknown scriptHash for " ++ (case swi of
                   ScriptWitnessIndexTxIn wo -> "Redeeming script at index : " ++ show wo
                   ScriptWitnessIndexMint wo -> "Minting Script at index: " ++ show wo
                   ScriptWitnessIndexCertificate wo -> "Certificate Script at index : " ++ show wo
                   ScriptWitnessIndexWithdrawal wo -> "Withdrawal Script at index  : " ++ show wo) --TODO Show script Hash too
                ScriptErrorMissingScript rp map -> makeErr  $ "Missing script : " ++ renderResolvablePointers map
                ScriptErrorMissingCostModel lan -> makeErr "Unexpected costModel Parameter Mismatch"
  where
          -- wits = case tx' of { ValidatedTx tb tw iv sm -> witsFromTxWitnesses tb  }
          makeErr t  = FrameworkError PlutusExecutionError t
          mkPlutusErr t = FrameworkError PlutusScriptError t
          renderResolvablePointers ::
            Map.Map
                RdmrPtr
                ( ScriptPurpose StandardCrypto
                , a
                , Ledger.ScriptHash StandardCrypto
                ) -> String
          renderResolvablePointers mp = intercalate ", " $ map (\( _, (sp, ma, sh)) -> show sp ++  show  sh)  $ Map.toList mp

          hex  :: BSL.ByteString -> String
          hex d = toHexString d