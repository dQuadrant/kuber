{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Kuber.Core.TxFramework where


import Cardano.Api hiding ( PaymentCredential)
import Cardano.Api.Shelley hiding (PaymentCredential)
import Cardano.Kuber.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Babbage.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Kuber.Util
import Data.Functor ((<&>))


import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSL
import Data.ByteString.Builder (charUtf8)

import Codec.Serialise (serialise)
import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, maybeToList)
import Data.List (intercalate, sortBy, minimumBy, find)
import qualified Data.Foldable as Foldable
import Plutus.V2.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, fromBuiltin)
import Cardano.Kuber.Core.TxBuilder
import Cardano.Kuber.Core.ChainInfo (DetailedChainInfo (DetailedChainInfo, dciConn), ChainInfo (getNetworkId, getConnectInfo, withDetails))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Cardano.Api.Crypto.Ed25519Bip32 (xPrvFromBytes)
import Debug.Trace (trace, traceM)
import qualified Data.Aeson as A
import GHC.Num (wordToInteger)
import qualified Data.Map.Strict as StrictMap
import qualified Debug.Trace as Debug
import Data.Aeson (ToJSON(toJSON))
import qualified Data.Text as T
import Data.Text.Conversions (convertText)
import Data.Word (Word64)
import Foreign.Storable (sizeOf)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Char as C
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as T
import Data.Int (Int64)
import Cardano.Api.Byron (Address(ByronAddress))
import Cardano.Ledger.Shelley.API (Credential(KeyHashObj), KeyHash (KeyHash))
import Cardano.Ledger.DescribeEras (StandardCrypto)
import Cardano.Binary (ToCBOR(toCBOR))
import qualified Cardano.Binary as Cborg
import Cardano.Kuber.Utility.ScriptUtil
import Cardano.Kuber.Utility.QueryHelper (queryUtxos, queryTxins)
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable(toConsoleTextNoPrefix))
import Cardano.Kuber.Utility.DataTransformation (skeyToPaymentKeyHash, pkhToPaymentKeyHash)
import Data.Foldable (foldlM)
import Data.Bifunctor (first)
import Cardano.Ledger.Coin (Coin(Coin))
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Key as A
import qualified Data.HashMap.Lazy as HMap
import Data.Time (nominalDiffTimeToSeconds)
import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import qualified Data.Text as Text
import Cardano.Ledger.Slot (EpochInfo, epochInfoFirst)
import Cardano.Slotting.EpochInfo (hoistEpochInfo, epochInfoSlotToUTCTime)
import Ouroboros.Consensus.HardFork.History.EpochInfo (interpreterToEpochInfo)
import           Control.Monad.Trans.Except(runExcept)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

type BoolChange   = Bool
type BoolFee = Bool
type  ParsedInput   = Either (Witness WitCtxTxIn BabbageEra,TxOut CtxUTxO BabbageEra ) (Maybe ExecutionUnits,ScriptWitness WitCtxTxIn BabbageEra ,TxOut CtxUTxO  BabbageEra )
type  ParsedOutput  = TxOutput (TxOut CtxTx BabbageEra)


txBuilderToTxBodyIO ::  ChainInfo i =>  i ->  TxBuilder  -> IO (Either FrameworkError  (TxBody BabbageEra))
txBuilderToTxBodyIO  a b  = txBuilderToTxBodyIO'  a b <&> (<&> fst)

txBuilderToTxBody ::DetailedChainInfo ->  UTxO BabbageEra -> TxBuilder   -> Either FrameworkError  (TxBody BabbageEra )
txBuilderToTxBody   a b c  =  txBuilderToTxBody' a b c <&> fst

txBuilderToTx::DetailedChainInfo ->  UTxO BabbageEra -> TxBuilder   -> Either FrameworkError  (Tx BabbageEra)
txBuilderToTx a b c = txBuilderToTxBody'  a b c <&> snd

txBuilderToTxIO :: ChainInfo i => i -> TxBuilder -> IO (Either FrameworkError (Tx BabbageEra))
txBuilderToTxIO a b   = txBuilderToTxBodyIO' a b   <&> ( <&> snd)


-- Given TxBuilder object, Construct a txBody
-- This IO code, constructs detailedChainInfo(protocolParam,costPerWord,eraHistory,SystemHistory)
-- then queries required utxos used in inputs and calls  txBuilderToTxBody
txBuilderToTxBodyIO'::  ChainInfo i =>  i ->  TxBuilder  -> IO (Either FrameworkError  (TxBody BabbageEra,Tx BabbageEra))
txBuilderToTxBodyIO' cInfo builder = do
  -- first determine the addresses and txins that need to be queried for value and address.
  let (selectionAddrs,sel_txins,sel_utxo) = mergeSelections
      (input_txins,input_utxo) = mergeInputs
      (txins,utxo) = ( sel_txins  <> input_txins <> collateralins <> referenceTxins, sel_utxo <> input_utxo <> collateralUtxo)
      (collateralins,collateralUtxo) = mergeColaterals
      addrs=   selectionAddrs  <> Set.fromList (mapMaybe getInputAddresses (txInputs builder))
  dcInfo <- withDetails cInfo
  -- query utxos of the addresses
  addrUtxos <- queryIfNotEmpty addrs (queryUtxos  conn addrs) (Right $ UTxO  Map.empty)
  case addrUtxos of
    Left fe -> pure $ Left fe
    Right (UTxO  uto) -> do
      let combinedUtxos = uto<> utxo
      let missingTxins= Set.difference txins ( Map.keysSet combinedUtxos )
      vals <- queryIfNotEmpty missingTxins (queryTxins conn missingTxins) (Right $ UTxO  Map.empty)
      case vals of
        Left fe -> pure $ Left fe
        Right (UTxO txInUtxos) ->do
          -- Compute Txbody and return
          pure $ txBuilderToTxBody' dcInfo (UTxO $ combinedUtxos <> txInUtxos) builder
  where

    queryIfNotEmpty v f v' = if null  v then pure v' else f
    conn=getConnectInfo cInfo
    mergeSelections=foldl mergeSelection (Set.empty,Set.empty ,Map.empty ) (txSelections builder)
    getInputAddresses :: TxInput -> Maybe AddressAny
    getInputAddresses x = case x of
      TxInputUnResolved (TxInputAddr aie) -> Just $ addressInEraToAddressAny aie
      TxInputUnResolved (TxInputSkey  skey) -> Just $ toAddressAny $ skeyToAddr skey  (getNetworkId cInfo) 
      _ -> Nothing

    mergeInputs = foldl  getInputTxins  (Set.empty,Map.empty) (txInputs  builder)
    getInputTxins :: (Set TxIn,Map TxIn (TxOut CtxUTxO BabbageEra)) -> TxInput -> (Set TxIn,Map TxIn (TxOut CtxUTxO BabbageEra))
    getInputTxins v@(ins,utxo) input = case input of
      TxInputResolved tir -> case tir of
        TxInputUtxo (UTxO uto) -> (ins, utxo <> uto)
        TxInputScriptUtxo tvs sd sd' m_eu (UTxO uto) -> (ins,utxo<>uto)
        TxInputReferenceScriptUtxo ref sd sd' m_eu (UTxO uto) -> (Set.insert ref ins,utxo<>uto)
      TxInputUnResolved tiur -> case tiur of
        TxInputTxin ti -> (Set.insert ti ins,utxo)
        TxInputAddr aie -> v
        TxInputScriptTxin tvs sd sd' m_eu ti -> (Set.insert ti ins, utxo)
        TxInputReferenceScriptTxin  ref sd sd' m_eu  ti -> (Set.insert ref $ Set.insert ti ins, utxo)
        TxInputSkey skey -> v
    mergeColaterals :: (Set TxIn,Map TxIn (TxOut CtxUTxO BabbageEra) )
    mergeColaterals  =foldl (\(s,m) collateral -> case collateral of
                    TxCollateralTxin ti -> (Set.insert ti s,m)
                    TxCollateralUtxo (UTxO uto) -> (s,uto <> m) ) (mempty,mempty) (txCollaterals builder)

    referenceTxins :: (Set TxIn)
    referenceTxins = foldl  (\s ref -> case ref of { TxInputReference ti -> Set.insert ti s }  ) Set.empty $   txInputReferences builder

    mergeSelection :: ( Set AddressAny,Set TxIn, Map TxIn (TxOut CtxUTxO BabbageEra))  -> TxInputSelection  -> (Set AddressAny,Set TxIn, Map TxIn (TxOut CtxUTxO BabbageEra))
    mergeSelection (a,i,u) sel = case sel of
        TxSelectableAddresses aies -> (Set.union a  (Set.fromList $ map addressInEraToAddressAny aies),i,u)
        TxSelectableUtxos (UTxO uto) -> (a,i, uto <> u)
        TxSelectableTxIn tis -> (a,Set.union i (Set.fromList tis),u)
        TxSelectableSkey skeys -> (Set.union a (Set.fromList $ map (\s ->  toAddressAny $ skeyToAddr s (getNetworkId   cInfo ) ) skeys), i , u )

-- Construct TxBody from TxBuilder specification.
-- Utxos map must be provided for the utxos that are available in wallet and used in input
txBuilderToTxBody'::DetailedChainInfo ->  UTxO BabbageEra -> TxBuilder   -> Either FrameworkError  (TxBody BabbageEra,Tx BabbageEra )
txBuilderToTxBody'  dCinfo@(DetailedChainInfo cpw conn pParam ledgerPParam systemStart eraHistory )
                    (UTxO availableUtxo)
                    (TxBuilder selections _inputs _inputRefs _outputs _collaterals validityStart validityEnd mintData extraSignatures explicitFee mChangeAddr metadata )
  = do
  --     (toLedgerPParams era pparams)
            --  tx
            --  (toLedgerUTxO era utxo)
            --  (toLedgerEpochInfo history)
            --  systemstart
            --  cModelArray
  let network = getNetworkId  dCinfo
  (resolvedMints, unresolvedMints) <- classifyMints (UTxO availableUtxo) mintData <&> partitionEithers
  let mergedMetadata = foldl injectMetadataPolicy (foldl  injectMetadataPolicy metadata resolvedMints) unresolvedMints
      injectMetadataPolicy :: Map Word64 A.Value -> TxMintData (PolicyId,a) -> Map Word64  A.Value
      injectMetadataPolicy _metaMap (TxMintData (policy,_) _ mp )
        = Map.foldlWithKey (\finalMap tag tokenNameMap -> Map.insert tag  (case Map.lookup tag finalMap of
                Nothing -> toJSON  (HMap.singleton policyHex tokenNameMap)
                Just (A.Object o) ->
                        toJSON ( HMap.insert (A.fromText policyHex) (toJSON tokenNameMap) (A.toHashMap o))
                _ ->    toJSON  (HMap.singleton policyHex tokenNameMap) )  finalMap)  _metaMap mp
        where
          policyHex = serialiseToRawBytesHexText policy
      insert m1 m2' = foldl (\m2 (k,v) -> case Map.lookup k m2 of
        Nothing -> m2
        Just any -> Map.insert k (any <> v) m2  ) m2'  $   Map.toList m1
  Debug.traceM ("MergedMetadata" ++ show mergedMetadata)
  Debug.traceM ("injectedMetadataPolicy" ++ show mergedMetadata)
  meta<- if null mergedMetadata
        then  Right TxMetadataNone
        else  do
          case metadataFromJson TxMetadataJsonNoSchema (toJSON $ splitMetadataStrings  metadata) of
            Left tmje -> Left $ FrameworkError BadMetadata  (show tmje)
            Right tm -> Right $ TxMetadataInEra  TxMetadataInBabbageEra tm
  resolvedInputs <- mapM resolveInputs _inputs
  fixedInputs <- usedInputs Map.empty (Right defaultExunits) resolvedInputs
  parsedOutputs <- mapM (parseOutputs network) _outputs
  fixedOutputs <-case updateTxOutMinAda (Lovelace cpw) parsedOutputs of
    Left (i, output,txoutAda,minAda) -> Left $ FrameworkError TxValidationError $  "$.outputs["++show i ++ "] Minimum lovelace txout is " ++ show minAda ++ ", But it has only " ++ show txoutAda
    Right tos -> pure tos
  collaterals <- if hasScriptInput
                  then  (case collaterals of
                    Nothing ->  Left $ FrameworkError BalancingError "No utxo available for collateral"
                    Just tis -> pure tis
                    )
                  else pure []
  let
      mintValue =  valueFromList $  foldl (\ l (TxMintData (p,_) amount _) -> l ++   map (first (AssetId p))  amount ) mempty  resolvedMints
      resolvedMintsMp = Map.fromList $ map (\(TxMintData (policyId,sw) _ _)->(policyId,sw)) resolvedMints
      txMintValue' postResolved = if null (valueToList mintValue) then TxMintNone  else  TxMintValue MultiAssetInBabbageEra mintValue $ BuildTxWith (resolvedMintsMp <> postResolved)
      fixedInputSum =  usedInputSum fixedInputs <> mintValue
      fee= Lovelace 300_000
      availableInputs = sortUtxos $ UTxO  $ Map.filterWithKey (\ tin _ -> Map.notMember tin fixedInputs) spendableUtxos
      calculator= computeBody meta (Lovelace cpw) compulsarySignatories  fixedInputSum availableInputs (map fst collaterals) fixedOutputs
      colalteralSignatories = Set.fromList ( map snd collaterals)
      withExtraSigs = appendExtraSignatures extraSignatures colalteralSignatories
      withMintSignatures = appendMintingScriptSignatures withExtraSigs (map (\(TxMintData (_,wit) _ _)-> wit) resolvedMints)
      compulsarySignatories = foldl (\acc x -> case x of
                          Left (_,TxOut a _ _ _) -> case addressInEraToPaymentKeyHash  a of
                                                    Nothing -> acc
                                                    Just pkh -> Set.insert pkh acc
                          Right _ -> acc ) withMintSignatures   $ Map.elems  fixedInputs
  mintWithDefaultExunits <- applyMintExUnits Map.empty  (\p -> pure defaultExunits) unresolvedMints
  (txBody1,signatories,fee1) <-  calculator  (txMintValue' mintWithDefaultExunits) fixedInputs   fee
  (finalBody,finalSignatories,finalFee) <- (
    if  not requiresExUnitCalculation && null  unresolvedMints
      then  (
        let iteratedBalancing 0 _ = Left $ FrameworkError LibraryError "Transaction not balanced even in 7 iterations"
            iteratedBalancing n lastFee= do
              case calculator (txMintValue' mempty) fixedInputs  lastFee  of
                Right  v@(txBody',signatories',fee') ->
                  if fee' ==  lastFee
                    then pure v
                    else iteratedBalancing (n-1)  fee'
                Left e -> Left e
        in iteratedBalancing  7  fee1
      )
      else (
          let iteratedBalancing 0 _ _ =   Left $ FrameworkError LibraryError "Transaction not balanced even in 10 iterations"
              iteratedBalancing n lastBody lastFee=do
                (inputExmap,mintExmap) <- evaluateExUnitMap dCinfo ( UTxO availableUtxo) lastBody
                inputs' <- usedInputs  (Map.map Right inputExmap ) (Right defaultExunits)  resolvedInputs
                exUnitAppliedMints <- applyMintExUnits mintExmap (\p -> Left $ FrameworkError BalancingError ("Missing Exunits for minting" ++ show p)) unresolvedMints
                case calculator (txMintValue' exUnitAppliedMints) inputs'  lastFee  of
                  Right  v@(txBody',signatories',fee') ->
                    if fee' ==  lastFee
                      then pure v
                      else iteratedBalancing (n-1) txBody'  fee'
                  Left e -> Left e
          in  iteratedBalancing  10 txBody1 fee1
        )
    )

  respond  finalBody finalSignatories
  where
    applyMintExUnits :: Map PolicyId ExecutionUnits
        -> (PolicyId -> Either FrameworkError ExecutionUnits)
        -> [TxMintData (PolicyId, ExecutionUnits -> ScriptWitness WitCtxMint BabbageEra)]
        -> Either FrameworkError (Map PolicyId (ScriptWitness WitCtxMint BabbageEra))
    applyMintExUnits mp onMissing unresolvedMints =  mapM  (\(TxMintData (p,f) _ _ ) -> case Map.lookup p mp of
      Nothing ->  onMissing p >>= (\x -> pure (p,f x))
      Just eu -> pure (p,f eu)   ) unresolvedMints <&> Map.fromList
    iterateFeeCalculation 0 _ _ _ = Left $ FrameworkError LibraryError "Transaction not balanced even in 7 iterations"
    iterateFeeCalculation n f txbody lastFee= do
      case f txbody  of
        Right  v@(txBody',signatories',fee') ->
          if fee' ==  lastFee
            then pure v
            else iterateFeeCalculation (n-1) f txBody' fee'
        Left e -> Left e

    selectableAddrs = foldl  (\s selection -> case selection of
            TxSelectableAddresses aies -> Set.fromList (map toShelleyAddr aies) <>s
            TxSelectableUtxos uto -> s
            TxSelectableTxIn tis -> s
            TxSelectableSkey sks -> Set.fromList (map (\x -> toShelleyAddr $ skeyToAddrInEra x (getNetworkId dCinfo)) sks) <> s  ) Set.empty  selections
    spendableUtxos = foldl (\mp (ti , tout@(TxOut addr _ _ _ ))-> if Set.member (toShelleyAddr addr) selectableAddrs then Map.insert ti tout mp else mp ) selectableUtxos  (Map.toList availableUtxo)
    selectableUtxos = foldl  (\s selection -> case selection of
          TxSelectableAddresses aies -> s
          TxSelectableUtxos (UTxO uto) -> s <> uto
          TxSelectableTxIn tin -> foldl (\s tin -> case Map.lookup tin availableUtxo of
            Nothing -> s
            Just any ->Map.insert tin any s  ) Map.empty tin
          TxSelectableSkey sks -> s  ) Map.empty  selections

    respond txBody signatories = pure (txBody,makeSignedTransaction (map (toWitness txBody) $ mapMaybe (`Map.lookup` availableSkeys) $ Set.toList signatories) txBody)
    toWitness body skey = makeShelleyKeyWitness body (WitnessPaymentKey skey)
    availableSkeys = foldl (\set v  -> case v of
      TxInputUnResolved (TxInputSkey sk) -> Map.insert  (skeyToPaymentKeyHash sk) sk set
      _ -> set ) selectableSkeys _inputs
    selectableSkeys =  Map.fromList $  map (\x -> (skeyToPaymentKeyHash x, x)) $  concat (mapMaybe (\case
        TxSelectableSkey sks -> Just sks
        _ -> Nothing )  selections) ++ mapMaybe (\case
      TxSignatureSkey sk -> Just sk
      _ -> Nothing) extraSignatures

    -- mapPolicyIdAndWitness :: TxMintData -> (PolicyId, ScriptWitness WitCtxMint BabbageEra)
    -- mapPolicyIdAndWitness (TxMintData pId sw _)= (pId, sw)

    hasScriptInput = any (\case
      TxInputResolved TxInputScriptUtxo {}-> True
      TxInputResolved TxInputReferenceScriptUtxo{}-> True
      TxInputUnResolved TxInputScriptTxin{} -> True
      TxInputUnResolved TxInputReferenceScriptTxin{} -> True
      _ -> False ) _inputs

    requiresExUnitCalculation = any (\case
      TxInputResolved TxInputScriptUtxo {}-> True
      TxInputResolved TxInputReferenceScriptUtxo{}-> True
      TxInputUnResolved TxInputScriptTxin{} -> True
      TxInputUnResolved TxInputReferenceScriptTxin{} -> True
      _ -> False ) _inputs


    -- unEitherExecutionUnit e= case e of
    --   Left e -> throw $  SomeError  $ "EvaluateExecutionUnits: " ++ show e
    --   Right v -> pure v
    appendExtraSignatures :: [TxSignature] -> Set (Hash PaymentKey) -> Set (Hash PaymentKey)
    appendExtraSignatures signatures _set = foldl (\set item -> case item of
        TxSignatureAddr aie -> case addressInEraToPaymentKeyHash   aie of
                                Just pkh -> Set.insert pkh set
                                Nothing -> set
        TxSignaturePkh pkh -> case pkhToPaymentKeyHash pkh of
                                Just pkh' -> Set.insert pkh' set
                                Nothing -> set
        TxSignatureSkey sk -> Set.insert (skeyToPaymentKeyHash   sk) _set
      ) _set signatures

    classifyMints mp  = mapM  (classifyMint mp)

    classifyMint :: UTxO era
      -> TxMintData TxMintingScriptSource
      -> Either FrameworkError
              (Either (TxMintData (PolicyId, ScriptWitness WitCtxMint BabbageEra))
                      (TxMintData (PolicyId,  ExecutionUnits -> ScriptWitness WitCtxMint BabbageEra)))
    classifyMint (UTxO mp)  (TxMintData source amount meta) = case source of
      TxMintingSimpleScript tss ->pure $ Left $ transform $ case tss of
        TxSimpleScriptV1 ss -> (PolicyId $ hashScript ( SimpleScript SimpleScriptV1 ss ), SimpleScriptWitness SimpleScriptV1InBabbage SimpleScriptV1 (SScript  ss))
        TxSimpleScriptV2 ss -> (PolicyId $ hashScript ( SimpleScript SimpleScriptV2 ss ), SimpleScriptWitness SimpleScriptV2InBabbage SimpleScriptV2 (SScript  ss))
      TxMintingPlutusScript tps m_eu sd ->
        let f = case tps of
              TxPlutusScriptV1 ps -> PlutusScriptWitness PlutusScriptV1InBabbage PlutusScriptV1 (PScript ps) NoScriptDatumForMint sd
              TxPlutusScriptV2 ps -> PlutusScriptWitness PlutusScriptV2InBabbage PlutusScriptV2 (PScript ps) NoScriptDatumForMint sd
            policy = PolicyId $ hashPlutusScript tps
        in case m_eu of
          Nothing -> pure $ Right $ transform (policy,f)
          Just eu -> pure $ Left $ transform (policy, f eu)
      TxMintingReferenceScript ti m_eu m_sd -> case Map.lookup ti mp of
              Nothing -> Left $ FrameworkError BalancingError  "Reference Script Utxo is missing"
              Just (TxOut _ _ _ (ReferenceScript _ anySc@(ScriptInAnyLang sl sc'))) ->do
                ScriptInEra langInEra script' <- validateScriptSupportedInEra' BabbageEra anySc
                case script' of
                  SimpleScript ssv ss -> pure $ Left $ transform (PolicyId $ hashScript script' , SimpleScriptWitness langInEra ssv (SReferenceScript  ti Nothing))
                  PlutusScript psv ps -> case m_sd of
                    Nothing -> Left $ FrameworkError WrongScriptType "Plutus script referenced but ScriptData is missing"
                    Just sd -> let  f =  PlutusScriptWitness langInEra psv (PReferenceScript ti Nothing) NoScriptDatumForMint sd
                                    policy = PolicyId $ hashScript script'
                                in pure $ case m_eu of
                                 Nothing -> Right $ transform (policy,f)
                                 Just eu -> Left $ transform (policy,f eu)
              Just _ -> Left $ FrameworkError BalancingError "Reference script Utxo used in minting doesn't have the script"
      where
        transform :: v -> TxMintData v
        transform v = TxMintData v amount meta


    appendMintingScriptSignatures :: Set (Hash PaymentKey) -> [ScriptWitness witctx BabbageEra]  ->   Set (Hash PaymentKey)
    appendMintingScriptSignatures    = foldl (\_set mints -> case mints of
        SimpleScriptWitness slie ssv (SScript ss) -> getScriptSignatures ss <> _set
        _ -> _set)
      where
        getScriptSignatures s = case  s of
          RequireSignature pkh -> Set.singleton pkh
          RequireTimeBefore tls sn -> mempty
          RequireTimeAfter tls sn -> mempty
          RequireAllOf sss -> foldMap getScriptSignatures sss
          RequireAnyOf sss -> foldMap getScriptSignatures sss
          RequireMOf n sss -> foldMap getScriptSignatures sss


    collaterals ::   Maybe [(TxIn,Hash PaymentKey )]
    collaterals   = case txContextCollaterals of
                          [] -> case mapMaybe canBeCollateral $ Map.toList spendableUtxos of
                            [] -> Nothing
                            v -> let  (tin,pkh,_) =minimumBy sortingFunc v in Just [(tin,pkh)]
                          v-> Just v
        where
        canBeCollateral :: (TxIn  , TxOut ctx BabbageEra) -> Maybe (TxIn, Hash PaymentKey, Integer)
        canBeCollateral v@(ti, to@(TxOut addr val mDatumHash _)) = case mDatumHash of
                              TxOutDatumNone -> case val of
                                TxOutAdaOnly _ (Lovelace v) ->  addressInEraToPaymentKeyHash  addr >>= (\pkh -> Just (ti,pkh,v))
                                TxOutValue _ va ->  let _list = valueToList va
                                                    in if length _list == 1
                                                        then  case addressInEraToPaymentKeyHash  addr of
                                                                Nothing -> Nothing
                                                                Just pkh -> Just ( ti,pkh,case snd $ head _list of { Quantity n -> n } )
                                                        else Nothing
                              _ -> Nothing
        filterCollateral = mapMaybe  canBeCollateral $ Map.toList spendableUtxos

        -- sort based on following conditions => Utxos having >4ada come eariler and the lesser ones come later.
        sortingFunc :: (TxIn,a,Integer) -> (TxIn,a,Integer)-> Ordering
        sortingFunc (_,_,v1) (_,_,v2)
          | v1 < 4 = if v2 < 4 then  v2 `compare` v1 else GT
          | v2 < 4 = LT
          | otherwise = v1 `compare` v2

    txContextCollaterals =foldl getCollaterals [] _collaterals
    getCollaterals  accum  x = case x  of
        TxCollateralTxin txin -> accum++ (case Map.lookup txin availableUtxo of
          Nothing -> error "Collateral input missing in utxo map"
          Just (TxOut a v dh _) -> case addressInEraToPaymentKeyHash  a of
                                    Just pkh ->  (txin,pkh) : accum
                                    Nothing -> error "Invalid address type utxo in collateral"
                                   )
        TxCollateralUtxo (UTxO mp) ->  accum ++ map (\(tin,TxOut a v dh _) -> case addressInEraToPaymentKeyHash a of
                                                                                 Just pkh -> (tin,pkh)
                                                                                 Nothing -> error "invalid address type utxo in collateral"
                      ) (Map.toList  mp)
    isJust (Just x)  = True
    isJust _ = False

    computeBody meta cpw signatories   fixedInputSum availableInputs collaterals fixedOutputs txMintValue' fixedInputs fee = do
      changeTxOut <-case findChange fixedOutputs of
        Nothing -> do
          changeaddr <- monadFailChangeAddr
          pure (TxOut changeaddr  ( TxOutValue MultiAssetInBabbageEra  (valueFromList [(AdaAssetId ,0)])) TxOutDatumNone ReferenceScriptNone )
        Just to -> pure to

      (extraUtxos,change) <- selectUtxosConsideringChange (babbageMinLovelace ledgerPParam) (toCtxUTxOTxOut  changeTxOut) availableInputs startingChange
      let
        maxChange = utxoListSum availableInputs <> startingChange
        missing = filterNegativeQuantity maxChange
        (feeUsed,changeUsed,outputs) = updateOutputs cpw  fee change fixedOutputs
        bodyContent allOutputs = mkBodyContent meta fixedInputs extraUtxos allOutputs collaterals txMintValue' fee
        requiredSignatories = foldl (\acc (_,TxOut a _ _ _) -> fromMaybe acc (addressInEraToPaymentKeyHash a <&> flip Set.insert acc)) signatories  extraUtxos
        signatureCount=fromIntegral $ length requiredSignatories
      bc <- if changeUsed
              then pure $ bodyContent outputs
              else do
                changeaddr <-  monadFailChangeAddr
                pure $ bodyContent (outputs++ [TxOut changeaddr (TxOutValue MultiAssetInBabbageEra change) TxOutDatumNone ReferenceScriptNone ])
      case makeTransactionBody bc of
          Left tbe ->Left  $ FrameworkError  LibraryError  (show tbe)
          Right tb -> do
            pure (tb,requiredSignatories,evaluateTransactionFee pParam tb signatureCount 0)

      where
        fixedOutputSum = foldMap txOutputVal fixedOutputs
          where
          txOutputVal :: ParsedOutput -> Value
          txOutputVal (TxOutput (TxOut _ (TxOutValue _ v) _ _) _  _ _) = v

        startingChange=   fixedInputSum <>   negateValue(fixedOutputSum<> if _hasFeeUtxo then mempty else lovelaceToValue fee )
        _hasFeeUtxo = any (\(TxOutput _ f _ _)->f) fixedOutputs


        monadFailChangeAddr= case mChangeAddr of
          Nothing ->  if null usableAddresses
                        then Left $ FrameworkError BalancingError "no change address"
                        else pure $ head usableAddresses

          Just aie -> pure aie
        usableAddresses :: [AddressInEra BabbageEra]
        usableAddresses=concat $ mapMaybe findInput selections
        findInput :: TxInputSelection ->Maybe [AddressInEra BabbageEra]
        findInput v= case v of
          TxSelectableAddresses aies -> Just aies
          TxSelectableUtxos (UTxO mp) -> Just $ map (\(TxOut aie tov tod _) -> aie ) $ Map.elems mp
          TxSelectableTxIn tis -> Just $ foldl   (\addrs x -> case Map.lookup x availableUtxo of
                    Nothing -> addrs
                    Just (TxOut aie tov tod _) -> aie: addrs) [] tis
          TxSelectableSkey sk -> Just $ foldl (\addrs sk -> addrs ++ [skeyToAddrInEra sk (getNetworkId dCinfo)]) [] sk
    getTxin :: Map TxIn ParsedInput -> [(TxIn,TxOut CtxUTxO BabbageEra )]-> [(TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn BabbageEra ))]
    getTxin v  v2 = map ( uncurry totxIn)  (Map.toList v) ++ map toPubKeyTxin v2

    toPubKeyTxin :: (TxIn,a) -> (TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn BabbageEra ))
    toPubKeyTxin (v1,v2) =(v1,BuildTxWith $ KeyWitness KeyWitnessForSpending )

    totxIn :: TxIn ->  ParsedInput -> (TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn BabbageEra ))
    totxIn  i  parsedInput = case parsedInput of
      Left (a,b) -> (i,BuildTxWith a)
      Right (e,a,b) -> (i,BuildTxWith  ( ScriptWitness ScriptWitnessForSpending a )  )
    mkBodyContent meta fixedInputs extraUtxos outs collateral  txMintValue' fee =  bodyContent --   Debug.trace ("Body Content :\n" ++ show bodyContent) bodyContent
      where
      references =  Set.fromList (map (\(TxInputReference a) -> a) _inputRefs) <>   referenceInputsFromScriptReference <> referenceInputsFromMint
      bodyContent=(TxBodyContent {
        txIns= getTxin fixedInputs extraUtxos ,
        txInsCollateral= if null collateral then TxInsCollateralNone  else TxInsCollateral CollateralInBabbageEra collateral,
        txOuts=outs,
        txInsReference = if Set.null references then TxInsReferenceNone   else  TxInsReference ReferenceTxInsScriptsInlineDatumsInBabbageEra $  Set.toList references  ,
        txTotalCollateral= TxTotalCollateralNone  ,
        txReturnCollateral = TxReturnCollateralNone ,
        Cardano.Api.Shelley.txFee=TxFeeExplicit TxFeesExplicitInBabbageEra  fee,
        txValidityRange= (txLowerBound,txUpperBound),
        Cardano.Api.Shelley.txMetadata=meta  ,
        txAuxScripts=TxAuxScriptsNone,
        txExtraKeyWits=keyWitnesses,
        txProtocolParams=BuildTxWith (Just  pParam),
        txWithdrawals=TxWithdrawalsNone,
        txCertificates=TxCertificatesNone,
        txUpdateProposal=TxUpdateProposalNone,
        txMintValue=txMintValue',
        txScriptValidity=TxScriptValidityNone
          })
    keyWitnesses = if null extraSignatures
                    then TxExtraKeyWitnessesNone
                    else TxExtraKeyWitnesses ExtraKeyWitnessesInBabbageEra $
                        foldl (\list x -> case x of
                            TxSignatureSkey sk -> skeyToPaymentKeyHash sk:list
                            TxSignatureAddr aie -> case addressInEraToPaymentKeyHash aie of
                              Nothing -> list
                              Just ha -> ha: list
                            TxSignaturePkh (PubKeyHash pkh) -> case
                                deserialiseFromRawBytes (AsHash AsPaymentKey) $ fromBuiltin pkh
                                    of
                                      Nothing -> list
                                      Just ha -> ha:list  ) [] extraSignatures

    -- fixedOutputSum = foldMap txOutputVal _outputs
    --   where
    --   txOutputVal :: TxOutput TxOutputContent -> Value
    --   txOutputVal o = case o of { TxOutput toc b b' _ -> case toc of
    --                                 TxOutAddress aie va -> va
    --                                 TxOutAddressWithReference aie va sa -> va
    --                                 TxOutScriptAddress aie va ha -> va
    --                                 TxOutPkh pkh va -> va
    --                                 TxOutScript tvs va ha -> va
    --                                 TxOutScriptAddressWithData _ va _ -> va
    --                                 TxOutScriptWithData _ va _ -> va
    --                             }
    zeroValue = valueFromList []

    referenceInputsFromScriptReference = foldl (\coll input  -> case input of
          TxInputResolved (TxInputReferenceScriptUtxo txin _ _ _ _)-> Set.insert txin coll
          TxInputUnResolved (TxInputReferenceScriptTxin txin _ _ _ _) -> Set.insert txin coll
          _ -> coll ) Set.empty  _inputs
    referenceInputsFromMint = foldl (\s (TxMintData tmss _ _) -> case tmss of
      TxMintingPlutusScript tps m_eu sd -> s
      TxMintingReferenceScript ti m_eu m_sd -> Set.insert ti s
      TxMintingSimpleScript tss -> s   ) Set.empty mintData
    findChange :: [ParsedOutput] -> Maybe (TxOut CtxTx BabbageEra )
    findChange ous =   find (\(TxOutput _ _ c _)  -> c ) ous <&> (\(TxOutput v _ _ _)-> v)
    updateOutputs cpw fee change outputs' = updateOutput cpw False False (getNetworkId  dCinfo) fee change outputs'
    updateOutput :: Lovelace -> BoolFee -> BoolChange -> NetworkId -> Lovelace -> Value -> [ParsedOutput] ->  (BoolFee,BoolChange,[TxOut CtxTx BabbageEra])
    updateOutput _ _ _ _ _ _ []  =  (False,False,[])
    updateOutput cpw _fUsed _cUsed network (Lovelace fee) change (txOutput:outs) =let
        (feeUsed,changeUsed,result) = transformOut _fUsed _cUsed txOutput
        (feeUsed2,changeUsed2,others) = updateOutput cpw feeUsed changeUsed network (Lovelace fee) change outs
        updatedOutput = (feeUsed  || feeUsed2 , changeUsed || changeUsed2, result : others )
        in   updatedOutput
      where
        transformOut feeUsed changeUsed  (TxOutput  tout@(TxOut aie v@(TxOutValue _ va) ha sref) addFee addChange minAdaAction)=
            (feeUsed',changeUsed',modifiedTxOut)
          where
            modifiedTxOut = TxOut aie (TxOutValue MultiAssetInBabbageEra changeNFeeIncluded) ha sref
            (feeUsed',feeIncluded) = includeFee va
            (changeUsed', changeNFeeIncluded) = includeChange feeIncluded

            -- deduct fee from the val if needed
            includeFee val
              | feeUsed = (True, val)
              | addFee = (True, valueFromList [(AdaAssetId ,Quantity (- fee))] <> val)
              | otherwise = (False,val)

            -- add change to the val if needed
            includeChange val
              | changeUsed = (True,val)
              | addChange = (True,change<> val)
              | otherwise = (False,val)
        transformOut _ _ _ = error "UnExpected condition"
    updateTxOutMinAda ::  Lovelace  -> [ParsedOutput] -> Either (Integer,ParsedOutput,Integer,Integer)   [ParsedOutput]
    updateTxOutMinAda cpw outs = foldlM  (addMinAdaIfNecessary cpw) [] $ zip [0..] outs

    addMinAdaIfNecessary ::  Lovelace  -> [ParsedOutput] -> (Integer,ParsedOutput) -> Either (Integer,ParsedOutput,Integer,Integer)   [ParsedOutput]
    addMinAdaIfNecessary  cpw collector  (i,t@(TxOutput txout@(TxOut add v@(TxOutValue era val) datum refScript) addFee addChange action))
      | addFee = doAdd t
      | addChange = doAdd t
      | otherwise  = let  Coin minLovelace =  babbageMinLovelace ledgerPParam (toCtxUTxOTxOut txout)
                          updatedTxout = updateLovelace newTxoutAda
                          Coin newMinLovelace = babbageMinLovelace ledgerPParam (toCtxUTxOTxOut updatedTxout)
                          newTxoutAda = (minLovelace- txOutAda)
                          Quantity txOutAda = selectAsset val AdaAssetId
                          doubleUpdate = transfrormOutput  t $  if newTxoutAda == newMinLovelace
                                                then updatedTxout
                                                else updateLovelace newMinLovelace
                      in  if txOutAda >= minLovelace
                            then doAdd t
                            else
                              case action of
                                  DropOnUtxoInsufficientUtxoAda -> Right collector
                                  IncreaseOnUtxoInsufficientUtxoAda -> doAdd  doubleUpdate
                                  ErrorOnInsufficientUtxoAda -> Left (i,t, txOutAda,minLovelace)
                                  OnInsufficientUtxoAdaUnset ->
                                    if txOutAda >0
                                      then  Left (i,t, txOutAda,minLovelace)
                                      else doAdd $ doubleUpdate
      where

        updateLovelace adaAmount =  TxOut add (TxOutValue era (val<> valueFromList [(AdaAssetId, Quantity adaAmount)]) ) datum refScript
        doAdd t = Right $ collector ++ [t]
    addMinAdaIfNecessary _ _ _ = error "Unexpected"

    parseOutputs ::  NetworkId -> TxOutput TxOutputContent -> Either FrameworkError   ParsedOutput
    parseOutputs  networkId output = case output of { TxOutput toc b b' _ -> case toc of
      TxOutScriptWithScript sc va sd tms ->
          transformer $ TxOut (plutusScriptAddr sc networkId)
                      (TxOutValue MultiAssetInBabbageEra va)
                      (TxOutDatumHash ScriptDataInBabbageEra sd)
                      (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra $ txScriptToScriptAny   tms)
      TxOutScriptWithDataAndScript sc va sd tms ->
          transformer $ TxOut (plutusScriptAddr sc networkId)
                      (TxOutValue MultiAssetInBabbageEra va)
                      (TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra   sd )
                      (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra $ txScriptToScriptAny  tms)
      TxOutScriptWithDataAndReference sc va sd ->
          transformer $ TxOut (plutusScriptAddr sc networkId)
                      (TxOutValue MultiAssetInBabbageEra va)
                      (TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra   sd )
                      (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra $ plutusScriptToScriptAny sc)
      TxOutNative to -> pure $ transfrormOutput output to
      TxOutPkh pkh va -> case pkhToMaybeAddr (getNetworkId  dCinfo) pkh of
        Nothing -> Left  $ FrameworkError ParserError  ("Cannot convert PubKeyHash to Address : "++ show pkh)
        Just aie ->  transformer $ TxOut aie  (TxOutValue MultiAssetInBabbageEra va ) TxOutDatumNone ReferenceScriptNone
      TxOutScript sc va ha ->
        transformer $ TxOut (plutusScriptAddr sc networkId)
                            (TxOutValue MultiAssetInBabbageEra va)
                            (TxOutDatumHash ScriptDataInBabbageEra ha )
                            ReferenceScriptNone
      TxOutScriptInline sc va ha ->
        transformer $ TxOut (plutusScriptAddr sc networkId)
                            (TxOutValue MultiAssetInBabbageEra va)
                            (TxOutDatumHash ScriptDataInBabbageEra ha )
                            (ReferenceScript ReferenceTxInsScriptsInlineDatumsInBabbageEra $ plutusScriptToScriptAny sc)
      TxOutScriptWithData  sc va sd ->
        transformer $ TxOut (plutusScriptAddr sc networkId)
                            (TxOutValue MultiAssetInBabbageEra  va)
                            (TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra  sd)
                            ReferenceScriptNone
                            }
      where
        transformer v = pure $ transfrormOutput output v
    resolveInputs ::  TxInput -> Either FrameworkError    TxInputResolved_
    resolveInputs v = case v of
      TxInputResolved tir -> pure tir
      TxInputUnResolved (TxInputTxin txin) ->  doLookup txin <&> TxInputUtxo
      TxInputUnResolved (TxInputAddr addr) ->   filterAddrUtxo addr <&> TxInputUtxo
      TxInputUnResolved (TxInputScriptTxin s d r exunit txin) -> doLookup txin <&>  TxInputScriptUtxo s d r exunit
      TxInputUnResolved (TxInputReferenceScriptTxin ref d r exunit txin) -> doLookup txin <&>  TxInputReferenceScriptUtxo ref d r exunit
      TxInputUnResolved (TxInputSkey sk) -> filterAddrUtxo (skeyToAddrInEra sk  (getNetworkId dCinfo)  ) <&> TxInputUtxo  

      where
        filterAddrUtxo addr =pure $ UTxO $ Map.filter (ofAddress addr) availableUtxo
        ofAddress addr (TxOut a _ _ _)= addr == a
        doLookup tin = case Map.lookup tin availableUtxo of
          Nothing -> Left $ FrameworkError LibraryError  "Input Utxo missing in utxo map"
          Just to ->pure $ UTxO $ Map.singleton  tin  to
    toInput ::  Map TxIn (Either ScriptExecutionError ExecutionUnits)  -> Either FrameworkError ExecutionUnits-> TxInputResolved_  -> Either FrameworkError   [(TxIn,ParsedInput)]
    toInput exUnitLookup onMissing inCtx = case inCtx of
      TxInputUtxo (UTxO txin) ->  pure $ map (\(_in,val) -> (_in,Left (  KeyWitness KeyWitnessForSpending, val) ))  $ Map.toList txin
      TxInputScriptUtxo sc mData r mExunit (UTxO txin) ->mapM (\(_in,val) ->  do
          exUnit <- getExUnit _in mExunit
          let witnessFunc = case sc of
                TxPlutusScriptV1 ps -> PlutusScriptWitness PlutusScriptV1InBabbage PlutusScriptV1 (PScript ps)
                TxPlutusScriptV2 ps -> PlutusScriptWitness PlutusScriptV2InBabbage PlutusScriptV2 (PScript ps)
          -- TODO check if the utxo in case of inline datum
          pure (_in,Right (mExunit, witnessFunc  (datumForTxin mData) r exUnit,val )) ) $ Map.toList txin
      TxInputReferenceScriptUtxo scriptRefTin mData r mExunit (UTxO txin) -> mapM (\(_in,val) -> do
        exUnit <- getExUnit _in mExunit
        case Map.lookup scriptRefTin availableUtxo of
          Nothing -> Left $ FrameworkError LibraryError "Missing utxo for reference script"
          Just (TxOut _ _ _ (ReferenceScript _ (ScriptInAnyLang sl sc))) ->do
              witness <-  createTxInReferenceScriptWitness scriptRefTin Nothing mData r exUnit
              pure (_in,Right (mExunit, witness,val ))
          Just _ ->Left $ FrameworkError BalancingError "Reference script utxo doesn't contain reference script"
              ) $ Map.toList txin

      where
        datumForTxin d = maybe InlineScriptDatum ScriptDatumForTxIn d
        getExUnit tin ex =case  ex of
          Just ex -> Right ex
          Nothing -> case Map.lookup tin exUnitLookup of
            Nothing -> onMissing
            Just e -> case e of
              Left see -> Left $ FrameworkError BalancingError (show see)
              Right eu -> Right eu

    usedInputs ::  Map TxIn (Either ScriptExecutionError ExecutionUnits) -> Either FrameworkError ExecutionUnits ->   [TxInputResolved_] -> Either FrameworkError  (Map TxIn ParsedInput)
    usedInputs exUnitLookup onMissing resolvedInputs = do
      vs<- mapM (toInput exUnitLookup onMissing) resolvedInputs
      pure $ Map.fromList $ concat vs
    usedInputSum :: Map TxIn ParsedInput -> Value
    usedInputSum mp =
      let parsedInputs= Map.elems mp
          inputValue v = case v of
            Left (_,TxOut  _ v _ _) -> txOutValueToValue v
            Right (_,_,TxOut  _ v _ _) -> txOutValueToValue v

      in foldMap inputValue $ Map.elems mp

    sortUtxos :: UTxO BabbageEra ->  [(TxIn,TxOut CtxUTxO BabbageEra )]
    sortUtxos  ( UTxO utxoMap) = sortBy sortingFunc ( Map.toList  $ Map.difference  utxoMap (Map.fromList collaterals)) ++ collaterals
        where
        collaterals = mapMaybe  (\(x,pkh) -> Map.lookup x utxoMap <&> (x,) )txContextCollaterals
        -- sort the txouts based on following condition
        -- - the ones with multiple assets comes first
        -- - then the ones with lower lovelace amount come
        -- - then the ones with higher lovelace amount come
        sortingFunc :: (TxIn,TxOut CtxUTxO BabbageEra) -> (TxIn,TxOut CtxUTxO BabbageEra)-> Ordering
        sortingFunc (_,TxOut _ (TxOutAdaOnly _ v1) _ _) (_, TxOut _ (TxOutAdaOnly _ v2)  _ _)         = v1 `compare` v2
        sortingFunc (_,TxOut _ (TxOutAdaOnly _ (Lovelace v))  _ _) (_, TxOut _ (TxOutValue _ v2) _ _) = LT
        sortingFunc (_,TxOut _ _ _ (ReferenceScript _ _)) (_, _)                                      =  LT
        sortingFunc (_,TxOut _ _ (TxOutDatumInline _ _) _) (_, _)                                     =  LT
        sortingFunc (_,_) (_, TxOut _ _ _ (ReferenceScript _ _))                                      =  GT
        sortingFunc (_, _) (_, TxOut _ _ (TxOutDatumInline _ _) _)                                    =  GT
        sortingFunc (_,TxOut _ (TxOutValue _ v1) _ _) (_, TxOut _ (TxOutAdaOnly _ v2) _ _)            =  GT
        sortingFunc (_,TxOut _ (TxOutValue _ v1) _ _) (_, TxOut _ (TxOutValue _ v2) _ _) =  let l1= length ( valueToList v1)
                                                                                                l2= length (valueToList v2) in
                                                                                        if l1==l2
                                                                                        then selectAsset v1 AdaAssetId `compare` selectAsset v2  AdaAssetId
                                                                                        else l2 `compare` l1

    -- from the utxos, try to remove utxos that can be removed while keeping the change positive or zero if possible
    selectUtxos u c = minimizeUtxos u (c <> utxoListSum u)
    minimizeUtxos utxos remainingChange= case utxos of
      []     -> ([] ,remainingChange)
      (txIn,txOut@(TxOut _ txOutVal _ _)):subUtxos -> if val `valueLte` remainingChange
              then   minimizeUtxos subUtxos newChange -- remove the current txOut from the list
              else (case minimizeUtxos subUtxos remainingChange of { (tos, va) -> ((txIn,txOut) :tos,va) }) -- include txOut in result
            where
              val = txOutValueToValue txOutVal
              newChange= remainingChange <> negateValue val

     -- consider change while minimizing i.e. make sure that the change has the minLovelace value.
    selectUtxosConsideringChange f txout  u c  = minimizeConsideringChange txout f u (c <> utxoListSum u)
    minimizeConsideringChange txout f available change= case filterNegativeQuantity change of
      [] -> Right $ if existingLove < minLove
                    then
                      (case Foldable.find (\(tin,utxo) -> extraLove utxo > (minLove - existingLove)) unmatched of
                        Just val ->  (fst matched ++ [val],snd matched <> txOutValue_ (snd val))
                        Nothing ->  matched
                      )
                    else
                      matched
      missing -> Left  $ FrameworkError  InsufficientInput $ "Missing Balance :" ++ show ( map (\(a,b)-> (a,-b)) missing)

      where
        matched@(utxos,newChange)=minimizeUtxos available change
        unmatched = filter   (\(k,_) -> k `notElem` matchedSet)   available
        matchedSet=Set.fromList $ map fst $  fst matched
        --Current Lovelace amount in the change utxo
        existingLove = case  selectAsset (snd  matched) AdaAssetId <> selectAsset  (txOutValue_ txout)  AdaAssetId   of
          Quantity n -> n
        --minimun Lovelace required in the change utxo
        minLove = case  f $ txoutWithChange (change <> valueFromList [(AdaAssetId,2_000_000_000)]) of
            Coin l -> l
        -- extra lovelace in this txout over the txoutMinLovelace
        extraLove utxo  = selectLove - minLoveInThisTxout
            where
              minLoveInThisTxout=case  f $ txoutWithChange val of
                  Coin l -> l
              val= txOutValue_ utxo
              selectLove = case selectAsset val AdaAssetId of { Quantity n -> n }
        txoutWithChange c = case txout of { TxOut addr v md _-> case v of
                                              TxOutAdaOnly oasie lo -> TxOut addr (TxOutValue MultiAssetInBabbageEra (lovelaceToValue lo <> c)) md ReferenceScriptNone
                                              TxOutValue masie va -> TxOut addr (TxOutValue MultiAssetInBabbageEra (va <> c)) md ReferenceScriptNone}

        txOutValue_ txout= case txout of { TxOut aie tov tod _-> txOutValueToValue tov }
    txLowerBound = case validityStart of
      NoValidityTime -> TxValidityNoLowerBound
      ValidityPosixTime ndt ->  TxValidityLowerBound ValidityLowerBoundInBabbageEra   (toSlot ndt)
      ValiditySlot sn -> TxValidityLowerBound ValidityLowerBoundInBabbageEra sn
    txUpperBound = case validityEnd of
      NoValidityTime ->  TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
      ValidityPosixTime ndt -> TxValidityUpperBound ValidityUpperBoundInBabbageEra (toSlot ndt)
      ValiditySlot sn -> TxValidityUpperBound ValidityUpperBoundInBabbageEra sn

    plutusWitness script _data redeemer exUnits = PlutusScriptWitness PlutusScriptV2InBabbage
                            PlutusScriptV2
                            script
                            (ScriptDatumForTxIn _data) -- script data
                            redeemer -- script redeemer
                            exUnits
    defaultExunits=ExecutionUnits {executionMemory=10000000,executionSteps= 6000000000 }
    -- isOnlyAdaTxOut (TxOut a v d) = case v of
    --                                     -- only ada then it's ok
    --                                     TxOutAdaOnly oasie (Lovelace lo) -> lo>=2500000
    --                                     -- make sure that it has only one asset and that one is ada asset.
    --                                     TxOutValue masie va -> length vals == 1 && snd(head vals) >= 2500000
    --                                           where
    --                                             vals=valueToList  va
    -- unWrapBalance f = do
      -- x <- f
      -- case  x  of
      --  Left tbe -> throw $ SomeError $ "First Balance :" ++ show tbe
      --  Right res -> pure res
    toSlot tStamp = case getNetworkId  dCinfo of
        Mainnet -> SlotNo $ fromIntegral $  mainnetSlot $ round tStamp
        Testnet nm -> SlotNo $ fromIntegral $ testnetSlot $ round tStamp
    testnetSlot timestamp= (timestamp -1607199617 )+ 12830401 -- using epoch 100 as refrence
    mainnetSlot timestamp = (timestamp -1596491091 )+ 4924800 -- using epoch 209 as reference

-- mkBalancedBody :: ProtocolParameters
--   -> UTxO BabbageEra
--   -> TxBodyContent BuildTx BabbageEra
--   -> Value
--   -> AddressInEra BabbageEra
--   -> Word
--   -> Either
--       TxBodyError
--       TxResult
-- mkBalancedBody  pParams (UTxO utxoMap)  txbody inputSum walletAddr signatureCount =
--     do
--       minLovelaceCalc <-case calculateTxoutMinLovelaceFunc pParams of
--         Nothing -> Left TxBodyMissingProtocolParams
--         Just f -> Right f

--       -- first iteration
--       let sanitizedOutputs = modifiedOuts minLovelaceCalc
--           (inputs1,change1) =minimize txouts  $ startingChange txouts sanitizedOutputs startingFee
--           txIns1=map utxoToTxBodyIn inputs1
--           bodyContent1=modifiedBody sanitizedOutputs (map utxoToTxBodyIn inputs1) change1 startingFee
--     --  error $ show $ map (txOutValueToValue  . txOutValue .snd) txouts
--       if not (positiveValue change1)
--         then
--           error $ "Insufficient balance : missing " ++ show change1
--         else
--           pure ()
--       txBody1 <- unEither $ case makeTransactionBody bodyContent1 of
--         Left tbe -> Left $ SomeError $ show tbe
--         Right tb -> Right  tb
--       let modifiedChange1=change1 <> negLovelace  fee1 <> lovelaceToValue startingFee
--           fee1= evaluateTransactionFee pParams txBody1 signatureCount 0
--           (inputs2,change2)= minimizeConsideringChange minLovelaceCalc txouts (startingChange txouts sanitizedOutputs fee1)
--           txIns2=map utxoToTxBodyIn inputs2
--           bodyContent2 =modifiedBody sanitizedOutputs txIns2 change2 fee1
--        -- if selected utxos are  sufficient to pay transaction fees, just use the fee and make txBody
--        -- otherwide, reselect txins and recalculate fee. it's very improbable that the we will need more txouts now
--       if positiveValue modifiedChange1 && isProperChange minLovelaceCalc modifiedChange1
--         then do
--           let  modifiedBody'=modifiedBody sanitizedOutputs txIns1 modifiedChange1 fee1
--           txBody<-makeTransactionBody modifiedBody'
--           Right (TxResult fee1 inputs1  modifiedBody'  txBody)
--         else do
--           txbody2 <- makeTransactionBody bodyContent2
--           let fee2=evaluateTransactionFee pParams txbody2 signatureCount 0
--               modifiedChange2 = change2 <> negLovelace fee2 <> lovelaceToValue fee1
--           if fee2 == fee1
--             then Right  (TxResult fee2 inputs2 bodyContent2 txbody2)
--             else do
--               if positiveValue modifiedChange2
--                 then (do
--                   let body3=modifiedBody sanitizedOutputs txIns2 modifiedChange2 fee2
--                   txBody3 <- makeTransactionBody body3
--                   Right (TxResult fee2 inputs2 body3 txBody3))
--                 else (do
--                    error $ "Insufficient balance : missing " ++ show modifiedChange2)


--   where
--   performBalance sanitizedOuts  change fee= do
--             let (inputs,change') =minimize txouts (change <> negLovelace fee)
--                 bodyContent=modifiedBody sanitizedOuts (map utxoToTxBodyIn inputs) change' fee
--             txBody1<-makeTransactionBody bodyContent

--             let modifiedChange1=change' <> negLovelace  fee' <> lovelaceToValue fee
--                 fee'= evaluateTransactionFee pParams txBody1 signatureCount 0
--                 (inputs2,change2)= minimize txouts modifiedChange1
--                 newBody =modifiedBody sanitizedOuts (map utxoToTxBodyIn inputs2) change2 fee'
--             if fee' == fee
--               then Right (bodyContent,change,fee)
--               else Right (newBody, modifiedChange1,fee')

--   startingFee=Lovelace $ toInteger $ protocolParamTxFeeFixed pParams

--   negLovelace v=negateValue $ lovelaceToValue v

--   utxosWithWitness (txin,txout) = (txin, BuildTxWith  $ KeyWitness KeyWitnessForSpending)


--   isProperChange f change = existingLove >  minLove
--     where
--       existingLove = case  selectAsset change AdaAssetId   of
--         Quantity n -> n
--       --minimun Lovelace required in the change utxo
--       minLove = case  f $ TxOut walletAddr (TxOutValue MultiAssetInBabbageEra change) TxOutDatumNone of
--           Lovelace l -> l


--   utxoToTxBodyIn (txIn,_) =(txIn,BuildTxWith $ KeyWitness KeyWitnessForSpending)

--   -- minimize' utxos remainingChange = (doMap,remainingChange)
--   --   where
--   --     doMap=map (\(txin,txout) -> tobodyIn txin) utxos
--   --     tobodyIn _in=(_in,BuildTxWith $ KeyWitness KeyWitnessForSpending)
--   --     val  out= txOutValueToValue $ txOutValue  out



--   -- change is whatever will remain after making payment.
--   -- At the beginning, we will assume that we will all the available utxos,
--   -- so it should be a +ve value, otherwise it means we don't have sufficient balance to fulfill the transaction
--   startingChange available outputs  fee=
--         negateValue (foldMap (txOutValueToValue  . txOutValue ) outputs)  --already existing outputs
--     <>   inputSum -- already existing inputs
--     <>  Foldable.foldMap (txOutValueToValue  . txOutValue . snd) available -- sum of all the available utxos
--     <>  negateValue (lovelaceToValue fee)
--   utxoToTxOut (UTxO map)=Map.toList map

--   txOutValueToValue :: TxOutValue era -> Value
--   txOutValueToValue tv =
--     case tv of
--       TxOutAdaOnly _ l -> lovelaceToValue l
--       TxOutValue _ v -> v

--   txOutValue (TxOut _ v _) = v

--   -- modify the outputs to make sure that the min ada is included in them if it only contains asset.
--   modifiedOuts calculator = map (includeMin calculator) (txOuts  txbody)
--   includeMin calculator txOut= do case txOut of {TxOut addr v hash-> case v of
--                                      TxOutAdaOnly oasie lo ->  txOut
--                                      TxOutValue masie va ->
--                                        if selectAsset va AdaAssetId == Quantity  0
--                                        then performMinCalculation addr va hash
--                                        else  txOut }
--     where
--       performMinCalculation addr val hash =TxOut  addr (TxOutValue MultiAssetInBabbageEra  (val <> lovelaceToValue minLovelace)) hash
--         where
--          minLovelace = minval addr (val <> lovelaceToValue (Lovelace 1_000_000)) hash

--       minval add v hash= calculator (TxOut add (TxOutValue MultiAssetInBabbageEra v) hash )

--   modifiedBody initialOuts txins change fee= content
--     where

--       content=(TxBodyContent  {
--             txIns= reorderInputs$ txins ++ txIns txbody,
--             txInsCollateral=txInsCollateral txbody,
--             txOuts=  if nullValue change
--                   then initialOuts
--                   else initialOuts ++ [ TxOut  walletAddr (TxOutValue MultiAssetInBabbageEra change) TxOutDatumNone]  ,
--             txFee=TxFeeExplicit TxFeesExplicitInBabbageEra  fee,
--             -- txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra),
--             txValidityRange = txValidityRange txbody,
--             txMetadata=txMetadata txbody ,
--             txAuxScripts=txAuxScripts txbody,
--             txExtraKeyWits=txExtraKeyWits txbody,
--             txProtocolParams= txProtocolParams   txbody,
--             txWithdrawals=txWithdrawals txbody,
--             txCertificates=txCertificates txbody,
--             txUpdateProposal=txUpdateProposal txbody,
--             txMintValue=txMintValue txbody,
--             txScriptValidity=txScriptValidity txbody
--           })

    -- v1Bundle= case case valueToNestedRep _v1 of { ValueNestedRep bundle -> bundle} of
    --   [ValueNestedBundleAda v , ValueNestedBundle policy assetMap] ->LovelaceToValue v
    --   [ValueNestedBundle policy assetMap]




-- mkTxExplicitFee ::DetailedChainInfo -> TxBuilder -> TxBody BabbageEra
-- mkTxExplicitFee = error "sad"

-- gatherInfo :: ChainInfo i -> i  -> TxBuilder  ->  IO (Either AcquireFailure TxContext)
-- gatherInfo cInfo  txBuilder@TxBuilder{txSelections, txInputs} = do
--   error "sad"
--   where

toLedgerEpochInfo :: EraHistory mode -> EpochInfo (Either Text.Text)
toLedgerEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (first (Text.pack . show) . runExcept) $
      interpreterToEpochInfo interpreter