{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Kuber.Utility.Misc where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Core.ChainAPI (HasChainQueryAPI (kQueryProtocolParams, kQuerySystemStart, kQueryUtxoByTxin))
import Cardano.Kuber.Core.Kontract (Kontract (KError))
import Cardano.Kuber.Error
import Cardano.Kuber.Utility.QueryHelper
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.UTxO (txins)
import Cardano.Slotting.Time (SystemStart, fromRelativeTime, toRelativeTime)
import Control.Exception (throw)
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Bifunctor (bimap)
import Data.ByteString.Builder (charUtf8)
import qualified Data.ByteString.Builder as BSL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as C
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import qualified Debug.Trace as Debug
import Ouroboros.Consensus.HardFork.History (unsafeExtendSafeZone)
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified PlutusLedgerApi.V1 as Plutus
import qualified Cardano.Ledger.Alonzo.Core as Ledger
import Control.Lens.Getter ((^.))
import Cardano.Ledger.Mary.Value as L (MaryValue (..), MultiAsset (MultiAsset), PolicyID (PolicyID))
import Cardano.Ledger.Alonzo.TxInfo (transPolicyID)
import Cardano.Ledger.Api (MaryEraTxBody(mintTxBodyL))


calculateTxoutMinLovelaceOrErr :: TxOut CtxTx ConwayEra -> ProtocolParameters -> Lovelace
calculateTxoutMinLovelaceOrErr t p = case calculateTxoutMinLovelace t p of
  Nothing -> error "Error calculating minlovelace"
  Just lo -> lo

calculateTxoutMinLovelace :: TxOut CtxTx ConwayEra -> ProtocolParameters -> Maybe Lovelace
calculateTxoutMinLovelace txout pParams = do
  bpparams <- case convertToLedgerProtocolParameters ShelleyBasedEraConway pParams  of
    Left ppce -> fail "Couldn't conver protocol parameters."
    Right bpp -> pure bpp
  pure $ calculateMinimumUTxO ShelleyBasedEraConway txout (unLedgerProtocolParameters bpparams)

babbageMinLovelace pparam txout = Ledger.getMinCoinTxOut pparam (toShelleyTxOut ShelleyBasedEraConway txout)

isNullValue :: Value -> Bool
isNullValue v = not $ any (\(aid, Quantity q) -> q > 0) (valueToList v)

isPositiveValue :: Value -> Bool
isPositiveValue v = not $ any (\(aid, Quantity q) -> q < 0) (valueToList v)

valueLte :: Value -> Value -> Bool
valueLte _v1 _v2 = not $ any (\(aid, Quantity q) -> q > lookup aid) (valueToList _v1) -- do we find anything that's greater than q
  where
    lookup x = case Map.lookup x v2Map of
      Nothing -> 0
      Just (Quantity v) -> v
    v2Map = Map.fromList $ valueToList _v2

filterNegativeQuantity :: Value -> [(AssetId, Quantity)]
filterNegativeQuantity v = filter (\(_, v) -> v < 0) $ valueToList v

txoutListSum :: [TxOut ctx era] -> Value
txoutListSum = foldMap toValue
  where
    toValue (TxOut _ val _ _) = case val of
      TxOutValue masie va -> va

utxoListSum :: [(a, TxOut ctx era)] -> Value
utxoListSum l = txoutListSum (map snd l)

utxoMapSum :: Map a (TxOut ctx era) -> Value
utxoMapSum x = txoutListSum $ Map.elems x

utxoSum :: UTxO ConwayEra -> Value
utxoSum (UTxO uMap) = utxoMapSum uMap

evaluateFee :: HasChainQueryAPI a => Tx ConwayEra -> Kontract a w FrameworkError Integer
evaluateFee tx = do
  pParam <- kQueryProtocolParams
  let txbody = getTxBody tx
      -- _inputs :: Set.Set TxIn
      -- _inputs = case txbody of ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn $ inputs tb
      (Lovelace fee) = evaluateTransactionFee shelleyBasedEra (unLedgerProtocolParameters pParam) txbody (fromIntegral $ length $ getTxWitnesses tx) 0
  pure fee

-- evaluateExUnitMap ::  HasChainQueryAPI a =>    TxBody ConwayEra -> Kontract a  w FrameworkError   (Map TxIn ExecutionUnits,Map PolicyId  ExecutionUnits)
-- evaluateExUnitMap  txbody = do
--   let
--       _inputs :: Set.Set TxIn
--       _inputs = case txbody of {ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn   $ inputs tb }
--   txIns <- kQueryUtxoByTxin  _inputs
--   evaluateExUnitMapWithUtxos txIns txbody

evaluateExUnitMapWithUtxos ::
  LedgerProtocolParameters ConwayEra ->
  SystemStart ->
  LedgerEpochInfo ->
  UTxO ConwayEra ->
  TxBody ConwayEra ->
  Either FrameworkError (Map TxIn ExecutionUnits, Map PolicyId ExecutionUnits)
evaluateExUnitMapWithUtxos protocolParams systemStart eraHistory usedUtxos txbody = do

  exMap <- case evaluateTransactionExecutionUnits
    systemStart
    eraHistory
    protocolParams
    usedUtxos
    txbody of
    Left tve -> Left $ FrameworkError ExUnitCalculationError (show tve)
    Right map -> pure map

  eithers <- mapM doMap (Map.toList exMap)
  pure $ bimap Map.fromList Map.fromList $ partitionEithers eithers
  where
    lTxBody = case txbody of ShelleyTxBody sbe tb scs tbsd m_ad tsv ->  tb
    inputList = case txbody of ShelleyTxBody sbe tb scs tbsd m_ad tsv -> Set.toList (txins tb)
    policyList  =  case txbody of { ShelleyTxBody sbe tb scs tbsd m_ad tsv -> case  tb ^. mintTxBodyL of { MultiAsset  mp ->  map  (\(PolicyID sh) -> PolicyId $ fromShelleyScriptHash sh )  $ Set.toAscList$  Map.keysSet mp } }

    inputLookup = Map.fromAscList $ zip [0 ..] inputList

    doMap (i, mExUnitResult) = case i of
      ScriptWitnessIndexTxIn wo -> do
        unEitherExUnits (fromShelleyTxIn (inputList !! fromIntegral wo),) mExUnitResult <&> Left
      ScriptWitnessIndexMint wo -> unEitherExUnits (policyList !! fromIntegral wo,) mExUnitResult <&> Right
      ScriptWitnessIndexCertificate wo -> Left $ FrameworkError FeatureNotSupported "Witness for Certificates is not supported"
      ScriptWitnessIndexWithdrawal wo -> Left $ FrameworkError FeatureNotSupported "Plutus script for withdrawl is not supported"

    unEitherExUnits f v = case v of
      Right e -> pure $ f e
      Left e -> case e of
        ScriptErrorEvaluationFailed ee txts -> Left (FrameworkError PlutusScriptError (T.unpack $ T.intercalate (T.pack ", ") txts))
        _ -> Left (FrameworkError ExUnitCalculationError (show e))

    transformIn (txIn, wit) exUnit =
      ( txIn,
        case BuildTxWith $ KeyWitness KeyWitnessForSpending of
          BuildTxWith wit' -> wit
      )

splitMetadataStrings :: Map Word64 A.Value -> Map Word64 A.Value
splitMetadataStrings = Map.map morphValue
  where
    morphValue :: A.Value -> A.Value
    morphValue val = case val of
      A.Object hm -> A.Object $ A.map morphValue hm
      A.Array vec -> A.Array (Vector.map morphValue vec)
      A.String txt -> let txtList = stringToList Vector.empty txt in if length txtList < 2 then A.String txt else A.Array txtList
      _ -> val

    -- Given a vector of Strings and Text, split the text into chunks of 64 bytes and append it into the vector as aeson String value.
    stringToList :: Vector.Vector A.Value -> T.Text -> Vector.Vector A.Value
    stringToList accum txt =
      let splitted = splitString 0 T.empty txt
          (prefix, remaining) = splitted -- Debug.trace ("splitString " ++ show txt ++ " : " ++ show splitted ) splitted
       in if T.null txt
            then accum
            else stringToList (Vector.snoc accum (A.String prefix)) remaining

    -- given prefix string and it's length, take characters from txt until prefix has size almost <=64 chars
    splitString :: Int64 -> T.Text -> T.Text -> (T.Text, T.Text)
    splitString size prefix txt =
      let tHead = T.head txt
          tHeadBS = LBS.length $ BSL.toLazyByteString $ toCharUtf8 tHead
          newSize = size + tHeadBS --Debug.trace ("Size of (" ++ (T.unpack prefix ) ++"," ++ if T.null txt then ""  else [tHead] ++  ") : " ++ show (size,newSize)) $
       in if T.null txt
            then (prefix, txt)
            else
              ( if newSize > 64
                  then
                    ( if C.isSpace tHead
                        then (prefix, txt)
                        else case splitOnLastSpace prefix of
                          (txt', Nothing) -> (prefix, txt)
                          (txtPre, Just txtEnd) -> (txtPre, T.concat [txtEnd, txt])
                    )
                  else splitString newSize (T.snoc prefix tHead) (T.tail txt)
              )
    -- given text try to find the last space and split it . Also make sure that the split is not too big :D
    splitOnLastSpace :: T.Text -> (T.Text, Maybe T.Text)
    splitOnLastSpace txt =
      let end = T.takeWhileEnd (not . C.isSpace) txt
          stripCount = T.length end
       in if stripCount <= 20
            then (T.dropEnd stripCount txt, Just end)
            else (txt, Nothing)

toCharUtf8 :: Char -> BSL.Builder
toCharUtf8 = charUtf8

timestampToSlot :: SystemStart -> EraHistory mode -> POSIXTime -> SlotNo
timestampToSlot sstart (EraHistory _ interpreter) utcTime = case Qry.interpretQuery (unsafeExtendSafeZone interpreter) (Qry.wallclockToSlot relativeTime) of
  -- left should never occur because we have used unsafeExtendSafeZone.
  Left phe -> error $ "Unexpected : " ++ show phe
  Right (slot, _, _) -> slot
  where
    relativeTime = toRelativeTime sstart (posixSecondsToUTCTime utcTime)

slotToTimestamp :: SystemStart -> EraHistory mode -> SlotNo -> POSIXTime
slotToTimestamp sstart (EraHistory _ interpreter) slotNo = case Qry.interpretQuery
  (unsafeExtendSafeZone interpreter)
  (Qry.slotToWallclock slotNo) of
  Left phe -> error $ "Unexpected : " ++ show phe
  Right (rt, _) -> utcTimeToPOSIXSeconds $ fromRelativeTime sstart rt
