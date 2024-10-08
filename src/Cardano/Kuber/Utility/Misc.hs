{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Kuber.Utility.Misc where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Kuber.Core.ChainAPI (HasChainQueryAPI (kQueryProtocolParams))
import Cardano.Kuber.Core.Kontract (Kontract (KError))
import Cardano.Kuber.Error
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Slotting.Time (fromRelativeTime, toRelativeTime)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.ByteString.Builder (charUtf8)
import qualified Data.ByteString.Builder as BSL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as C
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import Ouroboros.Consensus.HardFork.History (unsafeExtendSafeZone)
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import Control.Lens.Getter ((^.))
import qualified Cardano.Ledger.Api as Leger
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Api (MaryEraTxBody(..))
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Api.Ledger (Coin(unCoin))
import qualified Data.Foldable as Foldable
import qualified Cardano.Api.Ledger as L
import Data.Maybe (mapMaybe)
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra (bCardanoEra))


calculateTxoutMinLovelaceOrErr :: TxOut CtxTx ConwayEra -> ProtocolParameters -> Ledger.Coin
calculateTxoutMinLovelaceOrErr t p = case calculateTxoutMinLovelace t p of
  Nothing -> error "Error calculating minlovelace"
  Just lo -> lo

calculateTxoutMinLovelace :: TxOut CtxTx ConwayEra -> ProtocolParameters -> Maybe Ledger.Coin
calculateTxoutMinLovelace txout pParams = do
  bpparams <- case convertToLedgerProtocolParameters ShelleyBasedEraConway pParams  of
    Left ppce -> fail "Couldn't conver protocol parameters."
    Right bpp -> pure bpp
  pure $ calculateMinimumUTxO ShelleyBasedEraConway txout (unLedgerProtocolParameters bpparams)

txoutMinLovelace :: (IsCardanoEra era,IsShelleyBasedEra era) => Leger.PParams (ShelleyLedgerEra era) -> TxOut CtxUTxO era -> Ledger.Coin
txoutMinLovelace  = withCardanoEra cardanoEra
  where
    withCardanoEra :: CardanoEra era ->  Leger.PParams (ShelleyLedgerEra era) ->  TxOut CtxUTxO era  -> Ledger.Coin
    withCardanoEra era pparam txout = case era of  -- doing this to remove EraTXOut
      ShelleyEra -> txoutMinLovelace_ era pparam txout
      AllegraEra -> txoutMinLovelace_ era pparam txout
      MaryEra -> txoutMinLovelace_ era pparam txout
      AlonzoEra -> txoutMinLovelace_ era pparam txout
      BabbageEra -> txoutMinLovelace_ era pparam txout
      ConwayEra -> txoutMinLovelace_ era pparam txout
      _ -> Ledger.Coin 0
    txoutMinLovelace_ :: (Leger.EraTxOut (ShelleyLedgerEra era), IsShelleyBasedEra era) =>CardanoEra era ->  Leger.PParams (ShelleyLedgerEra era) -> TxOut CtxUTxO era -> Ledger.Coin
    txoutMinLovelace_  cera pparam txout = Ledger.getMinCoinTxOut pparam (toShelleyTxOut shelleyBasedEra txout)


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
      TxOutValueByron lo -> lovelaceToValue lo
      TxOutValueShelleyBased sbe va -> fromLedgerValue sbe va

utxoListSum :: [(a, TxOut ctx era)] -> Value
utxoListSum l = txoutListSum (map snd l)

utxoMapSum :: Map a (TxOut ctx era) -> Value
utxoMapSum x = txoutListSum $ Map.elems x

utxoSum :: UTxO era -> Value
utxoSum (UTxO uMap) = utxoMapSum uMap

evaluateFee :: HasChainQueryAPI a => Tx ConwayEra -> Kontract a w FrameworkError Integer
evaluateFee tx = do
  pParam <- kQueryProtocolParams
  let txbody = getTxBody tx
      -- _inputs :: Set.Set TxIn
      -- _inputs = case txbody of ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn $ inputs tb
      -- todo: FIX this fee calculation won't work when reference script is used.
      fee = evaluateTransactionFee shelleyBasedEra (unLedgerProtocolParameters pParam) txbody (fromIntegral $ length $ getTxWitnesses tx) 0 0
  pure (unCoin fee)

-- evaluateExUnitMap ::  HasChainQueryAPI a =>    TxBody ConwayEra -> Kontract a  w FrameworkError   (Map TxIn ExecutionUnits,Map PolicyId  ExecutionUnits)
-- evaluateExUnitMap  txbody = do
--   let
--       _inputs :: Set.Set TxIn
--       _inputs = case txbody of {ShelleyTxBody sbe tb scripts scriptData mAuxData validity -> Set.map fromShelleyTxIn   $ inputs tb }
--   txIns <- kQueryUtxoByTxin  _inputs
--   evaluateExUnitMapWithUtxos txIns txbody

--
-- 

evaluateExUnitMapWithUtxos :: IsTxBuilderEra era => SystemStart -> LedgerEpochInfo -> LedgerProtocolParameters era ->  UTxO era -> TxBody era -> Either      FrameworkError      (ExUnitResult era)
evaluateExUnitMapWithUtxos = evaluateExUnitMapWithUtxos_ bCardanoEra
  where
    evaluateExUnitMapWithUtxos_ :: CardanoEra era -> SystemStart -> LedgerEpochInfo -> LedgerProtocolParameters era -> UTxO era -> TxBody era -> Either      FrameworkError      (ExUnitResult era)
    evaluateExUnitMapWithUtxos_ bera  ss leInfo lPparam utxo txbody = do
        let     lTxBody = case txbody of ShelleyTxBody sbe tb scs tbsd m_ad tsv ->  tb
        exMap <- case evaluateTransactionExecutionUnits
              (toCardanoEra bera)
              ss
              leInfo
              lPparam
              utxo
              txbody of
          Left tve -> Left $ FrameworkError ExUnitCalculationError (show tve)
          Right map -> pure map
        case bera of
          BabbageEra -> do
            result <- evaluateExUnitMapWithUtxos__ txbody exMap
            result (\_ _ -> pure mempty)
          ConwayEra -> do
            result <- evaluateExUnitMapWithUtxos__ txbody exMap
            result extractPorposals

          _ ->Left (FrameworkError FeatureNotSupported "not in era")


data ExUnitResult era  =ExUnitResult  {
      exUnitResultInput :: Map TxIn ExecutionUnits
   ,  exUnitResultMint :: Map PolicyId ExecutionUnits
   ,  exUnitResultProposal :: Map (L.ProposalProcedure (ShelleyLedgerEra era)) ExecutionUnits
} 
emptyExUnitResult cardanoEraParam = ExUnitResult mempty mempty  (makeEmptyExmapResult cardanoEraParam)




makeEmptyExmapResult :: CardanoEra era ->   Map (L.ProposalProcedure (ShelleyLedgerEra era) ) ExecutionUnits 
makeEmptyExmapResult era = case era of 
  ConwayEra -> mempty
  BabbageEra -> mempty
  _ -> error "Error Unexpected"

evaluateExUnitMapWithUtxos__ txBody exMap= do

  inputs <- sequence $ mapMaybe  inputMap (Map.toList exMap)
  mints <- sequence $ mapMaybe  mintMap (Map.toList exMap)
  pure $ (\x -> do
    result <- x exMap txBody
    pure $ ExUnitResult (Map.fromList inputs) (Map.fromList mints) result  )
  where
    lTxBody = case txBody of ShelleyTxBody sbe tb scs tbsd m_ad tsv ->  tb
    inputList = Set.toAscList (lTxBody ^. Ledger.inputsTxBodyL)
    policyList   = case lTxBody  ^. mintTxBodyL of { MultiAsset  mp ->  map  (\(PolicyID sh) -> PolicyId $ fromShelleyScriptHash sh )  $ Set.toAscList$  Map.keysSet mp }

    inputMap (i, mExUnitResult) = case i of
        ScriptWitnessIndexTxIn wo ->
          Just $ unEitherExUnits txBody (fromShelleyTxIn (inputList !! fromIntegral wo),) mExUnitResult
        _ -> Nothing

    mintMap (i, mExUnitResult) = case i of
        ScriptWitnessIndexMint wo -> Just $  unEitherExUnits txBody (policyList !! fromIntegral wo,) mExUnitResult

        _ -> Nothing

unEitherExUnits ::TxBody era -> (ExecutionUnits -> (a, ExecutionUnits))
  -> Either ScriptExecutionError (b,ExecutionUnits)
  -> Either FrameworkError (a, ExecutionUnits)
unEitherExUnits txbody f v = case v of
  Right e -> pure $ f $ snd e
  Left e -> Left $ fromScriptExecutionError e txbody


extractPorposals exMap txBody = do
      result <- sequence $ mapMaybe proposalMap (Map.toList exMap)
      pure $ Map.fromList result
    where
      proposalList = Foldable.toList (lTxBody ^. Ledger.proposalProceduresTxBodyL)
      proposalMap (i, mExUnitResult) = case i of
        ScriptWitnessIndexProposing wo -> Just $  unEitherExUnits txBody (proposalList !! fromIntegral wo,) mExUnitResult
        _ -> Nothing
      lTxBody = case txBody of ShelleyTxBody sbe tb scs tbsd m_ad tsv ->  tb


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

timestampToSlot :: SystemStart -> EraHistory -> POSIXTime -> SlotNo
timestampToSlot sstart (EraHistory interpreter) utcTime = case Qry.interpretQuery (unsafeExtendSafeZone interpreter) (Qry.wallclockToSlot relativeTime) of
  -- left should never occur because we have used unsafeExtendSafeZone.
  Left phe -> error $ "Unexpected : " ++ show phe
  Right (slot, _, _) -> slot
  where
    relativeTime = toRelativeTime sstart (posixSecondsToUTCTime utcTime)

slotToTimestamp :: SystemStart -> EraHistory -> SlotNo -> POSIXTime
slotToTimestamp sstart (EraHistory interpreter) slotNo = case Qry.interpretQuery
  (unsafeExtendSafeZone interpreter)
  (Qry.slotToWallclock slotNo) of
  Left phe -> error $ "Unexpected : " ++ show phe
  Right (rt, _) -> utcTimeToPOSIXSeconds $ fromRelativeTime sstart rt

