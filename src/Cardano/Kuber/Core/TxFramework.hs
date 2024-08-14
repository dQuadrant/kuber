{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda-case" #-}
module Cardano.Kuber.Core.TxFramework where


import Cardano.Api hiding ( PaymentCredential)
import Cardano.Api.Shelley hiding (PaymentCredential)
import Cardano.Kuber.Error
    ( ErrorType(BalancingError, WrongScriptType, ParserError,
                LibraryError, InsufficientInput, BadMetadata, TxValidationError, FeatureNotSupported),
      FrameworkError(FrameworkError) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Either
import Data.Functor ((<&>))


import Data.Set (Set)
import Data.Maybe (mapMaybe, fromMaybe, fromJust)
import Data.List (sortBy, minimumBy, find)
import qualified Data.Foldable as Foldable
import PlutusLedgerApi.V2 (PubKeyHash(PubKeyHash), fromBuiltin)
import Cardano.Kuber.Core.TxBuilder
import qualified Data.Aeson as A
import Data.Aeson (ToJSON(toJSON))
import qualified Data.Text as T
import Data.Word (Word64)
import Cardano.Ledger.Shelley.API (Credential(KeyHashObj), KeyHash (KeyHash))
import Cardano.Kuber.Utility.ScriptUtil
import Cardano.Kuber.Utility.DataTransformation
import Data.Foldable (foldlM)
import Data.Bifunctor (first, Bifunctor (second))
import Cardano.Ledger.Coin (Coin(Coin))
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Key as A
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Text as Text
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Ouroboros.Consensus.HardFork.History.EpochInfo (interpreterToEpochInfo)
import Cardano.Kuber.Core.ChainAPI (HasChainQueryAPI (..))
import Cardano.Kuber.Utility.Misc
import Cardano.Kuber.Core.Kontract
import Cardano.Kuber.Core.TxScript

import Cardano.Kuber.Core.LocalNodeChainApi (HasLocalNodeAPI (..))
import Control.Lens ((^.))
import Cardano.Ledger.Api
    ( ProposalProcedure(..),
      GovAction(..) )
import Cardano.Api.Ledger (ConwayTxCert(..), ConwayDelegCert (..), PoolCert (..), ConwayGovCert (..), StrictMaybe (..), EraCrypto, Coin (unCoin), StandardCrypto)
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Kuber.Data.EraUpdate
import Cardano.Ledger.Api.PParams (ppKeyDepositL)
import Cardano.Ledger.Conway.PParams (ppDRepDepositL)
import qualified Cardano.Ledger.Conway.PParams as Ledger
import Cardano.Ledger.Conway.Governance ( ConwayEraGov (proposalsGovStateL,constitutionGovStateL),constitutionScriptL,proposalsGovStateL, pRootsL, PRoot (prRoot))

import qualified Data.OSet.Strict as OSet

import Cardano.Ledger.CertState (DRepState(DRepState))
import qualified Cardano.Api.Ledger as L
import Cardano.Kuber.Data.Models (fromStrictMaybe)
import qualified Debug.Trace as Debug

type BoolChange   = Bool
type BoolFee = Bool
type ChangeValue = Value
type FeeValue = Value

-- (  ScriptDatum witctx
--                          -> ScriptRedeemer
--                          -> ExecutionUnits
--                          -> ScriptWitness witctx era)

type PartialInput era = (TxIn,ExecutionUnits -> BuildTxWith BuildTx (Witness WitCtxTxIn era)  )
type ParsedInput era =  (TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn era))
type ParsedMint era = (PolicyId, ScriptWitness WitCtxMint era)
type PartialMint era = (PolicyId,ExecutionUnits -> ScriptWitness WitCtxMint era)

type PartialProposal era = (Proposal era , ExecutionUnits -> ScriptWitness WitCtxMint era)

type  ParsedOutput era  = TxOutput (TxOut CtxTx era)
type  ParseMintsF m era = Map PolicyId ExecutionUnits -> m TxMintValue BuildTx era
type  ParseInputsF m era =  Map TxIn ExecutionUnits -> [TxIn] -> m [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
type ParseProposalF m era = Map (L.ProposalProcedure (ShelleyLedgerEra era)) ExecutionUnits-> TxProposalProcedures BuildTx era


valueToRequiredEra:: CardanoEra era -> Value -> TxOutValue era
valueToRequiredEra cera val = case cera of
    MaryEra -> TxOutValueShelleyBased ShelleyBasedEraMary (toLedgerValue MaryEraOnwardsMary val)
    AlonzoEra ->  TxOutValueShelleyBased ShelleyBasedEraAlonzo (toLedgerValue MaryEraOnwardsAlonzo val)
    BabbageEra ->  TxOutValueShelleyBased ShelleyBasedEraBabbage (toLedgerValue MaryEraOnwardsBabbage val)
    ConwayEra ->  TxOutValueShelleyBased ShelleyBasedEraConway (toLedgerValue MaryEraOnwardsConway val)
    _ -> error "Unexpected"

getEra :: CardanoEra era1 -> ShelleyBasedEra era1
getEra era = case era of
  ShelleyEra -> ShelleyBasedEraShelley
  AllegraEra -> ShelleyBasedEraAllegra
  MaryEra -> ShelleyBasedEraMary
  AlonzoEra -> ShelleyBasedEraAlonzo
  BabbageEra -> ShelleyBasedEraBabbage
  ConwayEra -> ShelleyBasedEraConway
  _ -> error "Unexpected era"

-- Given TxBuilder object, Construct a txBody
-- This IO code, queries all the required parameters
-- then queries required utxos used in inputs, refInputs
-- then updates the deposit amounts and previousGovActionId if required.
-- finally it calls the pure txBuilderToTxBody function
executeTxBuilder::  (HasChainQueryAPI api ,HasLocalNodeAPI api,IsTxBuilderEra era)  =>    TxBuilder_ era  -> Kontract  api w FrameworkError (Cardano.Api.TxBody era,Tx era)
executeTxBuilder builder = do
  -- first determine the addresses and txins that need to be queried for value and address.
  network<- kGetNetworkId
  pParam <- kQueryProtocolParams
  systemStart <- kQuerySystemStart
  -- eraHistory <- kQueryEraHistory
  updatedProposals <- applyPrevGovActionIdNDeposit bConwayOnward (unLedgerProtocolParameters pParam) (txProposals builder)
  let (selectionAddrs,sel_txins,sel_utxo) = mergeSelections
      mergeSelections=foldl (mergeSelection network)  (Set.empty,Set.empty ,Map.empty ) (txSelections builder)
      (input_txins,input_utxo) = mergeInputs
      (txins,utxo) = ( sel_txins  <> input_txins <> collateralins <> referenceTxins, sel_utxo <> input_utxo <> collateralUtxo <> refUtxos)
      (collateralins,collateralUtxo) = mergeColaterals
      addrs=   selectionAddrs  <> Set.fromList (mapMaybe (getInputAddresses network) (txInputs builder))
  (UTxO  uto) <- queryIfNotEmpty addrs (kQueryUtxoByAddress   addrs) (UTxO  Map.empty)

  let combinedUtxos =  uto<> utxo
  let missingTxins= Set.difference txins ( Map.keysSet combinedUtxos )
  (UTxO txInUtxos) <- queryIfNotEmpty missingTxins (kQueryUtxoByTxin  missingTxins) (UTxO  Map.empty)
  let allUtxos = UTxO $  combinedUtxos <>  txInUtxos
  updatedCerts <- mapM  (updateCertDeposit (unLedgerProtocolParameters pParam)) (Cardano.Kuber.Core.TxBuilder.txCertificates builder)
  let updatedTxBuilder = txReplacePoposalsNCert builder updatedProposals updatedCerts
  eitherToKontract$  txBuilderToTxBody  network pParam systemStart (error "Cannot query era history") allUtxos  updatedTxBuilder

  where
    queryIfNotEmpty v f v' = if null  v then pure v' else f
    getInputAddresses :: NetworkId -> TxInput era -> Maybe AddressAny
    getInputAddresses network x = case x of
      TxInputUnResolved (TxInputAddr aie) -> Just $ addressInEraToAddressAny aie
      TxInputUnResolved (TxInputSkey  skey) -> Just $ toAddressAny $ skeyToAddr skey  network
      _ -> Nothing

    mergeInputs = foldl  getInputTxins  (Set.empty,Map.empty) (txInputs  builder)
    getInputTxins :: (Set TxIn,Map TxIn (TxOut CtxUTxO era)) -> TxInput era -> (Set TxIn,Map TxIn (TxOut CtxUTxO era))
    getInputTxins v@(ins,utxo) input = case input of
      TxInputResolved tir -> case tir of
        TxInputUtxo (UTxO uto) -> (ins, utxo <> uto)
        TxInputScriptUtxo tvs sd sd' m_eu (ti, tout) -> (ins, Map.insert ti tout utxo)
        TxInputReferenceScriptUtxo ref sd sd' m_eu (ti, tout) -> (Set.insert ref ins,Map.insert ti tout utxo)
      TxInputUnResolved tiur -> case tiur of
        TxInputTxin ti -> (Set.insert ti ins,utxo)
        TxInputAddr aie -> v
        TxInputScriptTxin tvs sd sd' m_eu ti -> (Set.insert ti ins, utxo)
        TxInputReferenceScriptTxin  ref sd sd' m_eu  ti -> (Set.insert ref $ Set.insert ti ins, utxo)
        TxInputSkey skey -> v

    mergeColaterals  =foldl (\(s,m) collateral -> case collateral of
                    TxCollateralTxin ti -> (Set.insert ti s,m)
                    TxCollateralUtxo (UTxO uto) -> (s,uto <> m) ) (mempty,mempty) (txCollaterals builder)

    refUtxos =  foldl  (\s ref -> case ref of
      TxInputReferenceTxin ti -> s
      TxInputReferenceUtxo (UTxO uto) -> uto <> s ) mempty $   txInputReferences builder

    referenceTxins :: (Set TxIn)
    referenceTxins = foldl  (\s ref -> case ref of
      TxInputReferenceTxin ti -> Set.insert ti s
      TxInputReferenceUtxo uto -> s ) Set.empty $   txInputReferences builder

    mergeSelection networkId (a,i,u) sel = case sel of
        TxSelectableAddresses aies -> (Set.union a  (Set.fromList $ map fromShelleyAddrToAny aies),i,u)
        TxSelectableUtxos (UTxO uto) -> (a,i, uto <> u)
        TxSelectableTxIn tis -> (a,Set.union i (Set.fromList tis),u)
        TxSelectableSkey skeys -> (Set.union a (Set.fromList $ map (\s ->  toAddressAny $ skeyToAddr s networkId ) skeys), i , u )

    applyPrevGovActionIdNDeposit :: HasChainQueryAPI api => Maybe (ConwayEraOnwards era) -> Ledger.PParams (ShelleyLedgerEra era) -> [TxProposal era] ->  Kontract api w FrameworkError [TxProposal era]
    applyPrevGovActionIdNDeposit beraOnward pParam props=
          case beraOnward of
            Just v@ConwayEraOnwardsConway ->  do
              govAction <- kQueryGovState
              let enactions =  govAction ^. proposalsGovStateL ^. pRootsL
              let constitutionScript = govAction ^. constitutionGovStateL ^. constitutionScriptL

              pure $ map
                   (\x -> case x of
                        TxProposal ppm -> TxProposal (updatePrevGovActionsNDeposit v enactions pParam constitutionScript ppm)
                        TxProposalScript ppm mex sc -> TxProposalScript  (updatePrevGovActionsNDeposit v enactions pParam constitutionScript ppm) mex sc
                        TxProposalScriptReference ppm mex sc ->
                          TxProposalScriptReference (updatePrevGovActionsNDeposit v enactions pParam constitutionScript ppm) mex sc
                      ) props


            Nothing -> pure props


    updatePrevGovActionsNDeposit eon enacedGovActions pParams constitutionScript (ProposalProcedureModal (ProposalProcedure (Coin co) ra ga an))  =let
        prevParamUpdate =  prRoot $ Ledger.grPParamUpdate enacedGovActions
        prevHardfork = prRoot $ Ledger.grHardFork enacedGovActions
        prevUpdateCommitted = prRoot $ Ledger.grCommittee enacedGovActions
        prevConstitution = prRoot $ Ledger.grConstitution enacedGovActions
        newga = case ga of
          ParameterChange sm ppu govScript -> ParameterChange (updateMaybe sm prevParamUpdate) ppu (updateMaybe govScript constitutionScript)
          HardForkInitiation sm pv -> HardForkInitiation (updateMaybe sm prevHardfork) pv
          TreasuryWithdrawals map govScript -> TreasuryWithdrawals map (updateMaybe govScript constitutionScript)
          NoConfidence sm ->  NoConfidence (updateMaybe sm prevUpdateCommitted)
          UpdateCommittee sm set map ui -> UpdateCommittee (updateMaybe sm prevUpdateCommitted) set map ui
          NewConstitution sm con -> NewConstitution (updateMaybe sm prevConstitution) con
          InfoAction -> ga
      in ProposalProcedureModal (ProposalProcedure (if co ==0 then pParams ^. Ledger.ppGovActionDepositL else Coin co) ra newga an)
    updateMaybe SNothing v =v
    updateMaybe v _ = v

    updateMaybe' SNothing v = SJust v
    updateMaybe' v _ = v

    updateCertDeposit :: HasChainQueryAPI api => Ledger.PParams (ShelleyLedgerEra era)  -> Certificate era  -> Kontract api w FrameworkError (Certificate era)
    updateCertDeposit pParam cert  =  case cert of
        ShelleyRelatedCertificate stbe stc -> pure cert
        ConwayCertificate ConwayEraOnwardsConway ctc -> updateCertConwayCertDeposit pParam cert

    updateCertConwayCertDeposit ::HasChainQueryAPI api => Ledger.PParams (ShelleyLedgerEra ConwayEra) -> Certificate ConwayEra ->  Kontract api w FrameworkError (Certificate ConwayEra)
    updateCertConwayCertDeposit  pParam cert = case cert of
      ShelleyRelatedCertificate stbe stc -> pure cert
      ConwayCertificate ceo ctc -> do
        val<- updateConwayCert  pParam ctc
        pure $ ConwayCertificate ceo val


    updateConwayCert :: (HasChainQueryAPI api , Ledger.ConwayEraPParams ledgerera,EraCrypto ledgerera ~ StandardCrypto) => Ledger.PParams ledgerera  ->  ConwayTxCert ledgerera   -> Kontract api w FrameworkError (ConwayTxCert ledgerera)
    updateConwayCert pParam ctc =  case ctc of
          ConwayTxCertDeleg cdc ->  (case cdc of
            ConwayRegCert cre sm -> pure $  ConwayRegCert cre  $ updateMaybe' sm (pParam ^. ppKeyDepositL)
            ConwayUnRegCert cre sm -> do
              case sm of
                SJust co -> pure $ ConwayUnRegCert cre sm
                SNothing -> do
                  result <- kQueryStakeDeposit (Set.singleton (fromShelleyStakeCredential cre) )
                  case Map.toList result of
                    [(cred, deposit)]->  pure $ ConwayUnRegCert cre (SJust  $  deposit)
                    _ -> kError TxValidationError $ "Stake address is not registered : " ++ show cre
            ConwayDelegCert cre del -> pure $ ConwayDelegCert cre del
            ConwayRegDelegCert cre del co -> pure $ ConwayRegDelegCert cre del (pParam ^.ppKeyDepositL )) <&> ConwayTxCertDeleg
          ConwayTxCertPool pc -> pure $ ConwayTxCertPool $ case pc of
            RegPool pp -> RegPool pp
            RetirePool kh en -> RetirePool kh en
          ConwayTxCertGov cgc ->   ( case cgc of
            ConwayRegDRep cre co@(Coin coinVal) sm ->
               pure $ ConwayRegDRep cre ( if coinVal==0 then pParam ^. ppDRepDepositL else co) sm
            ConwayUnRegDRep cre co@(Coin coinVal) ->do
              if coinVal /= 0
                then pure $ ConwayUnRegDRep cre co
              else do
                drepState <- kQueryDrepState (Set.singleton cre)

                case  Map.toList drepState of
                  [(cre', DRepState en sm co')]-> pure $ ConwayUnRegDRep cre co'
                  _ ->  kError TxValidationError $ "Drep  is not registered : " ++ show cre
            ConwayUpdateDRep cre mAnchor -> pure $ ConwayUpdateDRep cre mAnchor
            ConwayAuthCommitteeHotKey cre cre' -> pure $ ConwayAuthCommitteeHotKey cre cre'
            ConwayResignCommitteeColdKey cre _anchor-> pure $ ConwayResignCommitteeColdKey cre _anchor) <&> ConwayTxCertGov

-- Construct TxBody from TxBuilder specification.
-- Utxos map must be provided for the utxos that are available in wallet and used in input
txBuilderToTxBody:: (IsTxBuilderEra targetEra) =>
     NetworkId -> LedgerProtocolParameters targetEra -> SystemStart -> EraHistory
   ->  UTxO targetEra -> TxBuilder_ targetEra
   -> Either FrameworkError  (Cardano.Api.TxBody targetEra,Tx targetEra )
txBuilderToTxBody   network  pParam  systemStart eraHistory
                    ( UTxO availableUtxo)
                    txBuilder@(TxBuilder_ selections _inputs _inputRefs _outputs _collaterals validityStart validityEnd mintData extraSignatures proposals votes certs explicitFee mChangeAddr metadata auxScripts )
  = do
  (totalMintVal, mints_) <- parseMints (UTxO availableUtxo)

  let --mergedMetadata = foldl injectMetadataPolicy (foldl  injectMetadataPolicy metadata resolvedMints) _txMint
      (partialMints, parsedMints ) = partitionEithers mints_
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
  meta<- if null metadata -- TODO support metadata merging
        then  Right TxMetadataNone
        else  do
          case metadataFromJson TxMetadataJsonNoSchema (toJSON $ splitMetadataStrings  metadata) of
            Left tmje -> Left $ FrameworkError BadMetadata  (show tmje)
            Right tm -> Right $ TxMetadataInEra  shelleyBasedEra tm
  resolvedInputs <- mapM resolveInputs _inputs
  (hasScritProposals,pDeposits,parsedProposals) <- makeTxProposals txCardanoEra (UTxO availableUtxo) proposals
  (partialInputs,parsedInputs) <-  mapM parseResolvedInputs resolvedInputs <&> partitionEithers . concat
  parsedOutputs' <- mapM (parseOutputs network) _outputs
  fixedOutputs <-case updateTxOutMinAda  parsedOutputs' of
    Left (i, output,txoutAda,minAda) -> Left $ FrameworkError TxValidationError $  "$.outputs["++ show i ++ "] Minimum lovelace txout is " ++ show minAda ++ ", But it has only " ++ show txoutAda
    Right tos -> pure tos

  txChangeAddr <- monadFailChangeAddr
  collaterals <- if hasScriptInput || hasPlutusMint || hasScritProposals
                  then  (case maybeCollaterals of
                    Nothing ->  Left $ FrameworkError BalancingError "No utxo available for collateral"
                    Just tis -> pure tis
                    )
                  else pure []

  (txCerts,cDeposits) <- ( if null certs
    then pure $ (TxCertificatesNone,zeroValue)
    else inEonForEra
           (Left $ FrameworkError FeatureNotSupported "Certificate are not supported in Babbage era in Kuber")
           (\conwayOnward -> pure $    (
            Cardano.Api.Shelley.TxCertificates
              (conwayEraOnwardsToShelleyBasedEra conwayOnward)
              certs
              (BuildTxWith mempty),
              totalCertDeposits certs) ) txCardanoEra
    )
  validatedAuxScripts <- mapM (\(i,sc) ->  case toScriptInEra txShelleyBasedEra (txScriptToScriptAny sc) of
      Nothing -> Left $ FrameworkError FeatureNotSupported ("$.auxScripts[" ++ show i ++ "[] Script type is not supported in this era")
      Just scInEra -> Right scInEra
     )  (zip [0..] auxScripts)
  let
      toBuildTxWith v = map (second BuildTxWith)
      txBodyContentf1 ins  mints fProposals outs fee
          = (TxBodyContent {
        txIns=   parsedInputs ++ ins,
        txInsCollateral= if null collaterals then TxInsCollateralNone  else TxInsCollateral txAlonzoEraOnwards (map fst collaterals),
        txOuts=outs,
        txInsReference = if Set.null references then TxInsReferenceNone else TxInsReference bBabbageOnward (Set.toList references) ,
        txTotalCollateral= TxTotalCollateralNone  ,
        txReturnCollateral = TxReturnCollateralNone ,
        Cardano.Api.Shelley.txFee=TxFeeExplicit txShelleyBasedEra  fee,
        txValidityLowerBound = txLowerBound,
        txValidityUpperBound = txUpperBound,
        Cardano.Api.Shelley.txMetadata=meta,
        Cardano.Api.Shelley.txAuxScripts=if null auxScripts then TxAuxScriptsNone else TxAuxScripts txAllegraEraOnwards validatedAuxScripts  ,
        txExtraKeyWits=keyWitnesses,
        txProtocolParams=BuildTxWith (Just   pParam),
        txWithdrawals=TxWithdrawalsNone,
        Cardano.Api.Shelley.txCertificates= txCerts,
        txUpdateProposal=TxUpdateProposalNone,
        txMintValue=mints,
        txScriptValidity=TxScriptValidityNone,
        -- TODO : Parese proposal and voting proposals accordingly
        txProposalProcedures  = fProposals,
        txVotingProcedures  = parseToCApiVotingProcedures votes,
       txCurrentTreasuryValue = Nothing,
       -- | Treasury donation to perform
       txTreasuryDonation   = Nothing
      })

      parseToCApiVotingProcedures voting =
        case voting of
          [] -> Just $ Featured (fromJust bConwayOnward) TxVotingProceduresNone
          v -> Just $ Featured (fromJust bConwayOnward) (TxVotingProcedures votingProcedures (BuildTxWith mempty) )
        where
          mapOfGovernanceIdAndVotingProcedure = Map.fromList $ map (\(TxVote tvl) -> case tvl of { TxVoteL gai vp vo -> (gai,vp) }) voting
          votingProcedures = Ledger.VotingProcedures $ Map.fromList $ map (\(TxVote tvl)-> case tvl of { TxVoteL gai vp vo -> (vo,mapOfGovernanceIdAndVotingProcedure) }) voting

      txBodyContentf (ExUnitResult inputExUnits mintExUnits proposalExunits) onMissing extraIns touts fee   = do
        inputs <- applyExUnitToPartial inputExUnits onMissing partialInputs
        mints  <- applyExUnitToPartial mintExUnits  onMissing partialMints
        finalProposals <- parsedProposals proposalExunits onMissing
        pure $ txBodyContentf1 (inputs ++ toKeyWitnesses extraIns) (txMintValue' mints ) finalProposals touts fee

      txMintValue' postResolved =
          if null (valueToList totalMintVal)
            then TxMintNone
            else  TxMintValue txMaryEraOnwards totalMintVal  (BuildTxWith (  Map.fromList $ parsedMints <> postResolved ))
      builderInputUtxo = foldMap resolvedInputUtxo resolvedInputs
      fixedInputSum =   utxoMapSum builderInputUtxo <> totalMintVal <> negateValue (cDeposits <> pDeposits)

      startingFee=case explicitFee of
        Nothing ->  Coin 400_000
        Just n -> Coin n
      availableInputs = sortUtxos $ UTxO  $ Map.filterWithKey (\ tin _ -> Map.notMember tin builderInputUtxo) spendableUtxos
      calculator exunitResult onMissing=
        computeBody bBabbageOnward pParam (UTxO availableUtxo)
          (txBodyContentf exunitResult onMissing)
          txChangeAddr  compulsarySignatories
          fixedInputSum availableInputs fixedOutputs
      calculatorWithDefaults = calculator  (emptyExUnitResult txCardanoEra) (pure defaultExunits)
      colalteralSignatories = Set.fromList ( map snd collaterals)
      withExtraSigs = appendExtraSignatures  colalteralSignatories extraSignatures
      withMintSignatures = appendMintingScriptSignatures withExtraSigs $ map snd parsedMints
      withCertSignatures = appendCertSignatures withMintSignatures certs
      withVoteSigs = appendVotingSignatures withCertSignatures
      compulsarySignatories = foldl (\acc (TxOut a _ _ _) ->  case addressInEraToPaymentKeyHash  a of
                                                    Nothing -> acc
                                                    Just pkh -> Set.insert pkh acc
                            ) withVoteSigs   $ Map.elems  builderInputUtxo

  iteration1@(txBody1,signatories,fee1) <-  calculatorWithDefaults    startingFee
  let iterationFunc lastBody lastFee   =
        if  null partialInputs && null  partialMints && not hasScritProposals
          then calculatorWithDefaults  lastFee <&> makeNewFee
          else do
              let onMissing = Left $ FrameworkError LibraryError "Unexpected missing Exunits"
              exUnitResult <- evaluateExUnitMapWithUtxos systemStart (Cardano.Api.Shelley.toLedgerEpochInfo eraHistory) pParam   ( UTxO availableUtxo) lastBody
              let func = calculator exUnitResult onMissing
              v@(newBody,_,newFee) <-  func lastFee <&> makeNewFee 
              func newFee <&> makeNewFee     -- 2 consecutive call is required to prevent cycle in some transaction  
      iteratedBalancing n lastBody lastFee=do
        v@(txBody',signatories',fee')<- iterationFunc lastBody lastFee
        if (if n >0 then  (==) else (<=) ) fee'  lastFee
              then pure v
              else iteratedBalancing (n-1) txBody'  fee'
      respond (txBody,signatories,_) =  (txBody,makeSignedTransaction
                                            (map (toWitness txBody) $ mapMaybe (`Map.lookup` availableSkeys) $ Set.toList signatories)
                                            txBody
                                        )
    
      makeNewFee (a,b,fee) =case explicitFee of
        Nothing -> (a,b,fee)
        Just n -> (a,b,Coin n)
  iteratedBalancing  10 txBody1 fee1 <&> respond

  where
    applyExUnitToPartial :: Ord a => Map a ExecutionUnits -> Either FrameworkError ExecutionUnits->  [(a,ExecutionUnits -> b)] -> Either FrameworkError [(a, b)]
    applyExUnitToPartial mp onMissing pi = mapM (\(k,v)-> case Map.lookup k mp of
          Nothing -> do
            defaultExUnit <-onMissing
            pure (k, v defaultExUnit)
          Just eu -> pure (k, v eu)) pi

    applyMintExUnits :: Map PolicyId ExecutionUnits
        -> (PolicyId -> Either FrameworkError ExecutionUnits)
        -> [TxMintData (PolicyId, ExecutionUnits -> ScriptWitness WitCtxMint era)]
        -> Either FrameworkError (Map PolicyId (ScriptWitness WitCtxMint era))
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
            TxSelectableAddresses aies -> Set.fromList aies <>s
            TxSelectableUtxos uto -> s
            TxSelectableTxIn tis -> s
            TxSelectableSkey sks -> Set.fromList (map (\x -> toShelleyAddr $ skeyToAddrInEra @ConwayEra x network) sks) <> s  ) Set.empty  selections
    spendableUtxos = foldl (\mp (ti , tout@(TxOut addr _ _ _ ))-> if Set.member (toShelleyAddr addr) selectableAddrs then Map.insert ti tout mp else mp ) selectableUtxos  (Map.toList availableUtxo)

    selectableUtxos = foldl  (\s selection -> case selection of
          TxSelectableAddresses aies -> s
          TxSelectableUtxos (UTxO uto) -> s <> updateuMapEra  uto
          TxSelectableTxIn tin -> foldl (\s tin -> case Map.lookup tin availableUtxo of
            Nothing -> s
            Just any ->Map.insert tin any s  ) Map.empty tin
          TxSelectableSkey sks -> s  ) Map.empty  selections


    toWitness body skey = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentKey skey)
    availableSkeys = foldl (\set v  -> case v of
      TxInputUnResolved (TxInputSkey sk) -> Map.insert  (skeyToPaymentKeyHash sk) sk set
      _ -> set ) selectableSkeys _inputs
    selectableSkeys =  Map.fromList $  map (\x -> (skeyToPaymentKeyHash x, x)) $  concat (mapMaybe (\case
        TxSelectableSkey sks -> Just sks
        _ -> Nothing )  selections) ++ mapMaybe (\case
      TxSignatureSkey sk -> Just sk
      _ -> Nothing) extraSignatures

    -- mapPolicyIdAndWitness :: TxMintData -> (PolicyId, ScriptWitness WitCtxMint ConwayEra)
    -- mapPolicyIdAndWitness (TxMintData pId sw _)= (pId, sw)

    hasScriptInput = any (\case
      TxInputResolved TxInputScriptUtxo {}-> True
      TxInputResolved TxInputReferenceScriptUtxo{}-> True
      TxInputUnResolved TxInputScriptTxin{} -> True
      TxInputUnResolved TxInputReferenceScriptTxin{} -> True
      _ -> False ) _inputs

    hasPlutusMint = any (\case TxMintData tmss x0 map -> case tmss of
                                 TxMintingPlutusScript tps m_eu sd -> True
                                 TxMintingReferenceScript ti m_eu m_sd -> False -- TODO: find the reference script type here.
                                 TxMintingSimpleScript tss -> False
      ) mintData
    requiresExUnitCalculation = any (\case
      TxInputResolved TxInputScriptUtxo {}-> True
      TxInputResolved TxInputReferenceScriptUtxo{}-> True
      TxInputUnResolved TxInputScriptTxin{} -> True
      TxInputUnResolved TxInputReferenceScriptTxin{} -> True
      _ -> False ) _inputs


    appendExtraSignatures :: Set (Hash PaymentKey)  -> [TxSignature era] -> Set (Hash PaymentKey)
    appendExtraSignatures  = foldl (\set item ->
        let mCre= case item of
              TxSignatureAddr aie -> addressInEraToPaymentKeyHash   aie
              TxSignaturePkh pkh ->  pkhToPaymentKeyHash pkh
              TxSignatureSkey sk -> Just $ skeyToPaymentKeyHash   sk
        in case mCre of
              Just pkh' -> Set.insert pkh' set
              Nothing -> set
      )

    appendVotingSignatures old  = foldl  (\set (TxVote (TxVoteL gai vp vo)) ->
        let mCre=case vo of
                      Ledger.CommitteeVoter cre -> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
                      Ledger.DRepVoter cre -> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
                      Ledger.StakePoolVoter kh ->  Just $ fromLedgerKeyHash txShelleyBasedEra kh
        in case mCre of
              Just pkh' -> Set.insert pkh' set
              Nothing -> set
       ) old votes

    appendCertSignatures = foldl  (\set item -> case getPaymentKey item of
      Nothing -> set
      Just ha -> Set.insert ha set )

    getPaymentKey cert = case cert of
      ShelleyRelatedCertificate stbe stc -> Nothing
      ConwayCertificate ceo ctc -> case ctc of
        ConwayTxCertDeleg cdc -> case cdc of
          ConwayRegCert cre sm -> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
          ConwayUnRegCert cre sm -> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
          ConwayDelegCert cre del -> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
          ConwayRegDelegCert cre del co -> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
        ConwayTxCertPool pc -> case pc of
          RegPool pp -> Nothing
          RetirePool kh en -> Just $ fromLedgerKeyHash txShelleyBasedEra kh
        ConwayTxCertGov cgc -> case cgc of
          ConwayRegDRep cre co sm ->      ledgerCredToPaymentKeyHash txShelleyBasedEra cre
          ConwayUnRegDRep cre co ->       ledgerCredToPaymentKeyHash txShelleyBasedEra cre
          ConwayUpdateDRep cre sm ->       ledgerCredToPaymentKeyHash txShelleyBasedEra cre
          ConwayAuthCommitteeHotKey cre cre' -> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
          ConwayResignCommitteeColdKey cre _anchor-> ledgerCredToPaymentKeyHash txShelleyBasedEra cre
    txEra = toCardanoEra bBabbageOnward
    txCardanoEra=txEra
    txShelleyBasedEra = babbageEraOnwardsToShelleyBasedEra bBabbageOnward
    txMaryEraOnwards = babbageEraOnwardsToMaryEraOnwards bBabbageOnward
    txAlonzoEraOnwards = babbageEraOnwardsToAlonzoEraOnwards bBabbageOnward
    txAllegraEraOnwards = bAllegraOnward

    parseMints mp  = do
       result <- mapM  (resolveAndParseMint  mp) mintData
       let totalValue = foldMap fst result
       pure $ (totalValue,map snd result)

    getPaymentCre cre =  case cre of
        KeyHashObj (KeyHash ha) -> Just $  PaymentKeyHash (KeyHash ha)
        _ -> Nothing
    mapKeyHash  (KeyHash ha) = PaymentKeyHash (KeyHash ha)

    resolveAndParseMint :: ( IsTxBuilderEra era1) =>UTxO era1
          -> TxMintData TxMintingScriptSource
          -> Either FrameworkError
                  (Value, Either (PartialMint era1)
                                (ParsedMint era1)
                  )
    resolveAndParseMint  (UTxO mp)  (TxMintData source amount meta) =
      let toMintVal :: PolicyId -> Value
          toMintVal  p = valueFromList (map (\(ass,q) -> (AssetId p ass,q)) amount)


          result m_eu policy f =  case m_eu of
            Nothing -> pure $ (toMintVal policy, Left   (policy,f))
            Just eu -> pure $ (toMintVal policy,Right  (policy, f eu))

          transform :: v -> TxMintData v
          transform v = TxMintData v amount meta

      in
       case source of
        TxMintingSimpleScript txss ->let
            policy = txScriptPolicyId $ TxScriptSimple txss
          in do
            witness <- makeTxSimpleScriptWitness (getEra bCardanoEra) txss Nothing
            pure $ (toMintVal policy ,pure (policy,witness))

        TxMintingPlutusScript tps m_eu sd ->do
          witness <- makeTxPlutusScriptWitness shelleyBasedEra tps Nothing
          result m_eu (txScriptPolicyId $ TxScriptPlutus tps) (witness NoScriptDatumForMint sd)

        TxMintingReferenceScript ti m_eu m_sd -> case Map.lookup ti mp of
                Nothing -> Left $ FrameworkError BalancingError  $ "Reference Script Utxo is missing :" ++ T.unpack ( renderTxIn ti)
                Just (TxOut _ _ _ (ReferenceScript _ anySc@(ScriptInAnyLang sl sc'))) ->do
                  let txScript=txScriptFromScriptAny anySc
                      policy = txScriptPolicyId  txScript
                  witnessE <- makeTxScriptWitness shelleyBasedEra txScript (Just ti)
                  case witnessE of
                    Left f -> do
                      sd <- case m_sd of
                        Nothing -> Left $ FrameworkError WrongScriptType "Plutus script referenced but ScriptData is missing"
                        Just sd -> pure sd
                      result m_eu policy (f  NoScriptDatumForMint sd )
                    Right witness -> pure $( toMintVal policy,pure (policy, witness))
                Just _ -> Left $ FrameworkError BalancingError "Reference script Utxo used in minting doesn't have the script"

    appendMintingScriptSignatures :: Set (Hash PaymentKey) -> [ScriptWitness witctx era]  ->   Set (Hash PaymentKey)
    appendMintingScriptSignatures    = foldl (\_set mints -> case mints of
        SimpleScriptWitness slie (SScript ss) -> getScriptSignatures ss <> _set
        _ -> _set)
      where
        getScriptSignatures s = case  s of
          RequireSignature pkh -> Set.singleton pkh
          RequireTimeBefore sn -> mempty
          RequireTimeAfter sn -> mempty
          RequireAllOf sss -> foldMap getScriptSignatures sss
          RequireAnyOf sss -> foldMap getScriptSignatures sss
          RequireMOf n sss -> foldMap getScriptSignatures sss


    maybeCollaterals ::   Maybe [(TxIn,Hash PaymentKey )]
    maybeCollaterals   = case txContextCollaterals of
                          [] -> case mapMaybe canBeCollateral $ Map.toList spendableUtxos of
                            [] -> Nothing
                            v -> let  (tin,pkh,_) =minimumBy sortingFunc v in Just [(tin,pkh)]
                          v-> Just v
        where
        canBeCollateral :: (IsShelleyBasedEra era) => (TxIn  , TxOut ctx era) -> Maybe (TxIn, Hash PaymentKey, Integer)
        canBeCollateral v@(ti, to@(TxOut addr val mDatumHash _)) = case mDatumHash of
                              TxOutDatumNone -> case val of
                                TxOutValueByron ( v) ->  addressInEraToPaymentKeyHash  addr >>= (\pkh -> Just (ti,pkh,unCoin v))
                                TxOutValueShelleyBased sbe va ->  let _list = valueToList (fromLedgerValue sbe va)
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
          | v1 < 5 = if v2 < 5 then  v1 `compare` v2 else GT
          | v2 < 5 = LT
          | otherwise = v1 `compare` v2

    txContextCollaterals =foldl getCollaterals [] _collaterals
    getCollaterals  accum  x = case x  of
        TxCollateralTxin txin -> accum++ (case Map.lookup txin availableUtxo of
          Nothing -> error $ "Collateral input missing in utxo map : " ++ T.unpack ( renderTxIn txin)
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

    references :: Set TxIn
    references =
        foldl (\accum v -> case v of
            TxInputReferenceTxin tin -> Set.insert tin accum
            TxInputReferenceUtxo (UTxO utxo) -> Map.keysSet utxo <> accum
        ) mempty _inputRefs
        <> referenceInputsFromScriptReference
        <> referenceInputsFromMint


    monadFailChangeAddr= case mChangeAddr of
      Nothing ->  if null usableAddresses
                    then if null _inputs && null selections
                          then Left $ FrameworkError BalancingError "No utxo available for fee payment: both `inputs` and `selections` are empty"
                          else Left $ FrameworkError BalancingError "Change address is missing"
                    else pure $ head usableAddresses

      Just aie -> pure  $ updateAddressEra  aie

    usableAddresses=  foldr (\v addrs -> -- concat $ mapMaybe findInput selections
      case v of
        TxSelectableAddresses aies ->  map (fromShelleyAddr txShelleyBasedEra) aies ++ addrs
        TxSelectableUtxos (UTxO mp) -> map (\(TxOut aie tov tod _) -> updateAddressEra  aie ) (Map.elems mp) ++ addrs
        TxSelectableTxIn tis ->  foldl   (\addrs2 x -> case Map.lookup x availableUtxo of
                  Nothing -> addrs2
                  Just (TxOut aie tov tod _) -> aie: addrs2) addrs tis
        TxSelectableSkey sk ->   foldl (\addrs1 sk -> addrs1 ++ [skeyToAddrInEra sk network]) addrs sk ) [] selections

    txVoteToVotingProcedure = foldl processVoteMap mempty votes
      where
        processVoteMap accMap (TxVote ( TxVoteL govActionId votingProcedure voter)) =
          Map.insertWith const
              voter (Map.singleton govActionId votingProcedure) accMap


    updateTxOutMinAda  outs = foldlM  addMinAdaIfNecessary  [] $ zip [0..] outs

    addMinAdaIfNecessary   collector  (i,t@(TxOutput txout@(TxOut add v@(TxOutValueShelleyBased era val) datum refScript) addFee addChange action))
      | addFee = doAdd t
      | addChange = doAdd t
      | otherwise  = let  Coin minLovelace =  txoutMinLovelace ledgerPParam (toCtxUTxOTxOut txout)
                          updatedTxout = updateLovelace newTxoutAda
                          Coin newMinLovelace = txoutMinLovelace ledgerPParam (toCtxUTxOTxOut updatedTxout)
                          newTxoutAda = (minLovelace- txOutAda)
                          Quantity txOutAda = selectAsset (fromLedgerValue era val) AdaAssetId
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
                                      else doAdd  doubleUpdate
      where
        adaAmountToLedger era adaAmount = case
            valueToLovelace $ valueFromList [(AdaAssetId, Quantity adaAmount)]
          of
            Nothing -> error "error performing `valueToLovelace`"
            Just lo -> lovelaceToTxOutValue era lo
        updateLovelace adaAmount =  TxOut add ( valueToRequiredEra bCardanoEra (fromLedgerValue era val <> valueFromList [(AdaAssetId, Quantity adaAmount)]) ) datum refScript
        doAdd t = Right $ collector ++ [t]
    addMinAdaIfNecessary  _ _ = error "Cardano.Kuber.Core.TxFramework.txBuilderToTxBody.addMinAdaIfNecessary Impossible"


    sortUtxos :: UTxO era ->  [(TxIn,TxOut CtxUTxO era )]
    sortUtxos  ( UTxO utxoMap) = sortBy sortingFunc ( Map.toList  $ Map.difference  utxoMap (Map.fromList collaterals)) ++ collaterals
        where
        collaterals = mapMaybe  (\(x,pkh) -> Map.lookup x utxoMap <&> (x,) ) txContextCollaterals
        -- sort the txouts based on following condition
        -- - the ones with inline datum or script come at very first
        -- - the ones with multiple assets comes then
        -- - then the ones with lower lovelace amount come
        -- - then the ones with higher lovelace amount come
        sortingFunc :: (TxIn,TxOut CtxUTxO era) -> (TxIn,TxOut CtxUTxO era)-> Ordering
        sortingFunc (_,TxOut _ (TxOutValueByron v1) _ _) (_, TxOut _ (TxOutValueByron v2)  _ _)         = v1 `compare` v2
        sortingFunc (_,TxOut _ (TxOutValueByron ( v))  _ _) (_, TxOut _ (TxOutValueShelleyBased _ v2) _ _) = LT
        sortingFunc (_,TxOut _ _ _ (ReferenceScript _ _)) (_, _)                                      =  LT
        sortingFunc (_,TxOut _ _ (TxOutDatumInline _ _) _) (_, _)                                     =  LT
        sortingFunc (_,_) (_, TxOut _ _ _ (ReferenceScript _ _))                                      =  GT
        sortingFunc (_, _) (_, TxOut _ _ (TxOutDatumInline _ _) _)                                    =  GT
        sortingFunc (_,TxOut _ (TxOutValueShelleyBased _ v1) _ _) (_, TxOut _ (TxOutValueByron v2) _ _)            =  GT
        sortingFunc (_,TxOut _ (TxOutValueShelleyBased sbe1 v1) _ _) (_, TxOut _ (TxOutValueShelleyBased sbe2 v2) _ _) =
          let l1= length ( valueToList (fromLedgerValue sbe1 v1))
              l2= length (valueToList (fromLedgerValue sbe2 v2)) in
              if l1==l2
              then selectAsset (fromLedgerValue sbe1 v1) AdaAssetId `compare` selectAsset (fromLedgerValue sbe2 v2)  AdaAssetId
              else l2 `compare` l1
        -- insertVotingProcedure :: Ledger.GovActionId era -> Ledger.VotingProcedure era -> Map (Ledger.GovActionId era) (Ledger.VotingProcedure era) -> Map (Ledger.GovActionId era) (Ledger.VotingProcedure era)
        -- insertVotingProcedure govActionId votingProcedure newMap = Map.insertWith (\_ old -> old) govActionId votingProcedure newMap

-- Example usage:
-- let result = txVoteToVotingProcedure yourListOfTxVotes
    -- totalDeposits = map (\(Proposal v) -> case v of ) proposals
    -- certificates = map (\x ->  ) certs
    keyWitnesses = if null extraSignatures
                    then TxExtraKeyWitnessesNone
                    else TxExtraKeyWitnesses txAlonzoEraOnwards $
                        foldl (\list x -> case x of
                            TxSignatureSkey sk -> skeyToPaymentKeyHash sk:list
                            TxSignatureAddr aie -> case addressInEraToPaymentKeyHash aie of
                              Nothing -> list
                              Just ha -> ha: list
                            TxSignaturePkh (PubKeyHash pkh) -> case
                                deserialiseFromRawBytes (AsHash AsPaymentKey) $ fromBuiltin pkh
                                    of
                                      Left _ -> list
                                      Right ha -> ha:list  ) [] extraSignatures

    referenceInputsFromScriptReference = foldl (\coll input  -> case input of
          TxInputResolved (TxInputReferenceScriptUtxo txin _ _ _ _)-> Set.insert txin coll
          TxInputUnResolved (TxInputReferenceScriptTxin txin _ _ _ _) -> Set.insert txin coll
          _ -> coll ) Set.empty  _inputs
    referenceInputsFromMint = foldl (\s (TxMintData tmss _ _) -> case tmss of
      TxMintingPlutusScript tps m_eu sd -> s
      TxMintingReferenceScript ti m_eu m_sd -> Set.insert ti s
      TxMintingSimpleScript tss -> s   ) Set.empty mintData



    parseOutputs  networkId output = case output of { TxOutput toc b b' _ -> case toc of
      TxOutScriptWithScript sc va sd tms ->
          transformer $ TxOut (plutusScriptAddr sc networkId)
                      (valueToRequiredEra bCardanoEra va)
                      (TxOutDatumHash txAlonzoEraOnwards sd)
                      (ReferenceScript bBabbageOnward $ txScriptToScriptAny   tms)
      TxOutScriptWithDataAndScript sc va sd tms ->
          transformer $ TxOut (plutusScriptAddr sc networkId)
                      (valueToRequiredEra bCardanoEra va)
                      (TxOutDatumInline bBabbageOnward   sd )
                      (ReferenceScript bBabbageOnward $ txScriptToScriptAny  tms)
      TxOutScriptWithDataAndReference sc va sd ->
          transformer $ TxOut (plutusScriptAddr sc networkId)
                      (valueToRequiredEra bCardanoEra va)
                      (TxOutDatumInline bBabbageOnward   sd )
                      (ReferenceScript bBabbageOnward $ plutusScriptToScriptAny sc)
      TxOutNative to -> pure $ transfrormOutput output (updateTxOutInEra'  to)
      TxOutPkh pkh va -> case pkhToMaybeAddr network pkh of
        Nothing -> Left  $ FrameworkError ParserError  ("Cannot convert PubKeyHash to Address : "++ show pkh)
        Just aie ->  transformer $ TxOut aie  (valueToRequiredEra bCardanoEra va) TxOutDatumNone ReferenceScriptNone
      TxOutScript sc va ha ->
        transformer $ TxOut (plutusScriptAddr sc networkId)
                            (valueToRequiredEra bCardanoEra va)
                            (TxOutDatumHash txAlonzoEraOnwards ha )
                            ReferenceScriptNone
      TxOutScriptInline sc va ha ->
        transformer $ TxOut (plutusScriptAddr sc networkId)
                            (valueToRequiredEra bCardanoEra va)
                            (TxOutDatumHash txAlonzoEraOnwards ha )
                            (ReferenceScript bBabbageOnward $ plutusScriptToScriptAny sc)
      TxOutScriptWithData  sc va sd ->
        transformer $ TxOut (plutusScriptAddr sc networkId)
                            (valueToRequiredEra bCardanoEra va)
                            (TxOutDatumInline bBabbageOnward  sd)
                            ReferenceScriptNone
                            }
      where
        transformer v = pure $ transfrormOutput output  v
    resolvedInputUtxo :: TxInputResolved_ era -> Map TxIn (TxOut CtxUTxO era)
    resolvedInputUtxo  =  \case
      TxInputUtxo (UTxO utxo) -> utxo
      TxInputScriptUtxo tps m_hsd hsd m_eu v -> Map.fromAscList [v]
      TxInputReferenceScriptUtxo ti m_hsd hsd m_eu v -> Map.fromAscList [v]

    resolveInputs (v ) = case v of
      TxInputResolved resin ->  pure $ case resin of
        TxInputUtxo uto -> TxInputUtxo $ updateUtxoEra uto
        TxInputScriptUtxo tps m_hsd hsd m_eu (tin,tout) -> TxInputScriptUtxo tps m_hsd hsd m_eu (tin,updateTxOutInEra tout)
        TxInputReferenceScriptUtxo ti m_hsd hsd m_eu (tin,tout) -> TxInputReferenceScriptUtxo ti m_hsd hsd m_eu (tin,updateTxOutInEra tout)
      TxInputUnResolved (TxInputTxin txin) ->  doLookup txin <&> uncurry Map.singleton  <&>   TxInputUtxo . UTxO
      TxInputUnResolved (TxInputAddr addr) ->   filterAddrUtxo (updateAddressEra addr) <&> TxInputUtxo
      TxInputUnResolved (TxInputScriptTxin sc d r exunit txin) -> do
          --  TODO : Look into this 
          -- ScriptInEra langInEra script' <- validateScriptSupportedInEra' bShelleyBasedEra (txScriptToScriptAny $ TxScriptPlutus sc)
          doLookup txin <&>  TxInputScriptUtxo sc d r exunit
      TxInputUnResolved (TxInputReferenceScriptTxin ref d r exunit txin) -> doLookup txin <&>  TxInputReferenceScriptUtxo ref d r exunit
      TxInputUnResolved (TxInputSkey sk) -> filterAddrUtxo (skeyToAddrInEra sk  network  ) <&> TxInputUtxo

      where
        filterAddrUtxo addr =pure $ UTxO $ Map.filter (ofAddress addr) availableUtxo
        ofAddress addr (TxOut a _ _ _)= addr == a
        doLookup tin = case Map.lookup tin availableUtxo of
          Nothing -> Left $ FrameworkError LibraryError $  "Input Utxo missing in utxo map : " ++ T.unpack (renderTxIn tin)
          Just to ->pure $ (tin,to)

    toKeyWitnesses ins = map (\_in -> (_in, BuildTxWith $ KeyWitness KeyWitnessForSpending ))  ins
    parseResolvedInputs inCtx =
      let
        withExUnits witF mData r  mExUnit (_in, _out) = case mExUnit of
            Nothing -> pure [Left (_in,BuildTxWith . ScriptWitness ScriptWitnessForSpending . witF  (datumForTxin mData) r)]
            Just eu -> pure [Right (_in,  BuildTxWith $ ScriptWitness ScriptWitnessForSpending $  witF  (datumForTxin mData) r eu)]
        datumForTxin d =  ScriptDatumForTxIn d -- TODO: check if this one works for inline datum or not.
      in case inCtx of
      TxInputUtxo (UTxO txin) ->  pure $ map (\(_in,_) -> Right (_in, BuildTxWith $ KeyWitness KeyWitnessForSpending ))  $ Map.toList txin
      TxInputScriptUtxo sc mData r mExunit tout ->do
          witnessF <- makeTxPlutusScriptWitness shelleyBasedEra sc Nothing
          -- TODO check if the utxo has datum  in case of inline datum
          withExUnits witnessF mData r mExunit tout

      TxInputReferenceScriptUtxo scriptRefTin mData r mExunit tout@( _in,_out) ->  do
        case Map.lookup scriptRefTin availableUtxo of
          Nothing -> Left $ FrameworkError LibraryError $ "Missing reference script utxo: " ++ T.unpack (renderTxIn scriptRefTin)
          Just (TxOut _ _ _ (ReferenceScript _ (ScriptInAnyLang sl sc))) ->do
              case sc of
                SimpleScript ss -> Left $ FrameworkError WrongScriptType $ "Expected Plutus Reference script, but is a Simple script in this input: " ++ T.unpack (renderTxIn scriptRefTin)
                PlutusScript psv ps -> do
                  witnessF <- case psv of
                      PlutusScriptV1 -> makeTxPlutusScriptWitness shelleyBasedEra ( toTxPlutusScript ps   ) (Just _in)
                      PlutusScriptV2 -> makeTxPlutusScriptWitness shelleyBasedEra ( toTxPlutusScript ps   ) (Just _in)
                      PlutusScriptV3 -> makeTxPlutusScriptWitness shelleyBasedEra ( toTxPlutusScript ps   ) (Just _in)
                  withExUnits witnessF mData r mExunit tout
          Just _ ->Left $ FrameworkError BalancingError $ "Utxo used as refreence script doesn't contain reference script: " ++ T.unpack (renderTxIn scriptRefTin)


    txLowerBound = case validityStart of
      NoValidityTime -> TxValidityNoLowerBound
      ValidityPosixTime ndt -> TxValidityLowerBound txAllegraEraOnwards  (toSlot ndt)
      ValiditySlot sn -> TxValidityLowerBound txAllegraEraOnwards sn

    txUpperBound = case validityEnd of
      NoValidityTime ->  TxValidityUpperBound txShelleyBasedEra Nothing
      ValidityPosixTime ndt -> TxValidityUpperBound txShelleyBasedEra (Just$ toSlot ndt)
      ValiditySlot sn -> TxValidityUpperBound txShelleyBasedEra (Just sn)

    defaultExunits=ExecutionUnits {executionMemory=100000,executionSteps= 60000000 }

    toSlot  =  timestampToSlot systemStart  eraHistory
    ledgerPParam = unLedgerProtocolParameters pParam

    totalCertDeposits ::[Certificate era] -> Value
    totalCertDeposits certs= lovelaceToValue $ Coin $ sum (map certDeposit certs)
      where
      certDeposit cert =  case cert of
        ShelleyRelatedCertificate stbe stc -> 0
        ConwayCertificate ceo ctc -> case ctc of
          ConwayTxCertDeleg cdc -> case cdc of
            ConwayRegCert cre sm ->maybeToCoin sm
            ConwayUnRegCert cre sm -> negate $  maybeToCoin sm
            ConwayDelegCert cre del -> 0
            ConwayRegDelegCert cre del (Coin co) -> co
          ConwayTxCertPool pc -> case pc of
            RegPool pp -> 0
            RetirePool kh en -> 0
          ConwayTxCertGov cgc -> case cgc of
            ConwayRegDRep cre (Coin co) sm -> co
            ConwayUnRegDRep cre (Coin co) -> negate co
            ConwayUpdateDRep cre sm -> 0
            ConwayAuthCommitteeHotKey cre cre' -> 0
            ConwayResignCommitteeColdKey cre _anchor-> 0
      maybeToCoin sm = case sm of
            SNothing -> 0
            SJust (Coin co) -> co




toLedgerEpochInfo :: EraHistory -> EpochInfo (Either Text.Text)
toLedgerEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first (Text.pack . show) . runExcept) $
      interpreterToEpochInfo interpreter


-- txBuilderFromTx :: Tx ConwayEra -> TxBuilder
-- txBuilderFromTx tx = let
--       ledgerTx= case tx of { ShelleyTx sbe tx' -> tx'   }
--       ShelleyTxBody _ txBody scripts scriptData mAuxData scriptValidity   =  getTxBody tx
--       inputs =  mconcat $ map
--               txConsumeTxIn
--             $ Set.toList $  Set.map fromShelleyTxIn  $ txBody ^. inputsTxBodyL

--       outputs_ :: [TxOut CtxTx ConwayEra]
--       outputs_ = map  (fromShelleyTxOut shelleyBasedEra) $  toList  $ txBody ^. outputsTxBodyL

--       outputs =  foldMap (\x -> txOutput$ TxOutput (TxOutNative $  x) False False ErrorOnInsufficientUtxoAda )  outputs_

--       proposals = txBody ^. proposalProceduresTxBodyL
--     in mempty




fromLedgerKeyHash_ :: KeyHash r Ledger.StandardCrypto -> Hash PaymentKey
fromLedgerKeyHash_ (KeyHash kh) = PaymentKeyHash ( KeyHash kh )

fromLedgerKeyHash :: forall targetEra (r :: Ledger.KeyRole). ShelleyBasedEra targetEra ->  KeyHash r (EraCrypto (ShelleyLedgerEra targetEra)) -> Hash PaymentKey
fromLedgerKeyHash cera = case cera of
  ShelleyBasedEraShelley -> fromLedgerKeyHash_
  ShelleyBasedEraAllegra -> fromLedgerKeyHash_
  ShelleyBasedEraMary -> fromLedgerKeyHash_
  ShelleyBasedEraAlonzo -> fromLedgerKeyHash_
  ShelleyBasedEraBabbage -> fromLedgerKeyHash_
  ShelleyBasedEraConway -> fromLedgerKeyHash_



    -- -> BuildTxWith build (Map (L.ProposalProcedure (ShelleyLedgerEra era)) (ScriptWitness WitCtxStake era))
    --  PlutusScriptWitness :: ScriptLanguageInEra lang era
    --                      -> PlutusScriptVersion lang
    --                      -> PlutusScriptOrReferenceInput lang
    --                      -> ScriptDatum witctx
    --                      -> ScriptRedeemer
    --                      -> ExecutionUnits
    --                      -> ScriptWitness witctx era

unitRedeemer = unsafeHashableScriptData $ ScriptDataConstructor 0 []

makeTxProposals :: (Eon eon, IsTxBuilderEra era) =>  CardanoEra era  -> UTxO era ->  [TxProposal era] ->
      Either  FrameworkError
        (Bool,Value, Map (L.ProposalProcedure (ShelleyLedgerEra era)) ExecutionUnits ->
                Either FrameworkError ExecutionUnits
              -> Either FrameworkError (Maybe (Featured eon era (TxProposalProcedures BuildTx era)))
        )
makeTxProposals  conOnward  (UTxO utxos) proposals=do
  case conOnward of
      v@ConwayEra ->
        let totalDeposit = totalProposalDeposits proposals
            unProcedures= mapMaybe (\x -> case x of
              TxProposal (ProposalProcedureModal pp) ->  Just pp
              _ -> Nothing
              ) proposals
            makeWitness :: TxProposal ConwayEra
              -> Maybe
                  (Either
                      FrameworkError (ProposalProcedure (ShelleyLedgerEra ConwayEra), Either (ExecutionUnits -> ScriptWitness WitCtxStake ConwayEra)  (ScriptWitness WitCtxStake ConwayEra) ))
            makeWitness  x =  case x of
              TxProposalScript (ProposalProcedureModal pp) eu sc ->Just $ do
                witness <- makeTxPlutusScriptWitness shelleyBasedEra sc Nothing
                let partial = witness NoScriptDatumForStake unitRedeemer
                Right $ (pp,case eu of
                  Nothing -> Left partial
                  Just aeu -> Right $ partial aeu)

              TxProposalScriptReference (ProposalProcedureModal pp) eu ti -> Just ( do
                  partial <- case Map.lookup ti utxos of
                      Nothing -> Left $ FrameworkError BalancingError  $ "Reference Script Utxo is missing :" ++ T.unpack ( renderTxIn ti)
                      Just (TxOut _ _ _ (ReferenceScript _ anySc@(ScriptInAnyLang sl sc'))) ->do
                        let txScript=txScriptFromScriptAny anySc
                            scriptHash = txScriptHash  txScript
                        witnessE <- makeTxScriptWitness shelleyBasedEra txScript (Just ti)
                        case getUsedScriptHash pp of
                          Nothing -> Left $ FrameworkError BalancingError "Script provided for proposal, but its not required"
                          Just actonScHash -> if scriptHash /= actonScHash
                            then  Left (FrameworkError BalancingError "Script provided for reference has different hash from the constitution")
                            else
                              case witnessE of
                                Left f -> do
                                    Right $  f  NoScriptDatumForStake (unsafeHashableScriptData $ ScriptDataConstructor 0 [])
                                Right witness ->  Left $ FrameworkError WrongScriptType "Simple script referenced in proposal"
                      Just _ -> Left $ FrameworkError BalancingError "Reference script Utxo used in proposal doesn't have the script"
                  Right $ case eu of
                    Nothing -> (pp,Left partial)
                    Just aeu -> (pp,Right $ partial aeu)
                  )
              _ -> Nothing

            getUsedScriptHash  (ProposalProcedure (Coin co) ra ga an)  = case ga of
                    ParameterChange sm ppu govScript ->  fromStrictMaybe  govScript <&> fromShelleyScriptHash
                    TreasuryWithdrawals map govScript -> fromStrictMaybe govScript <&> fromShelleyScriptHash
                    _ -> Nothing
            scriptProcedures :: Either FrameworkError [(ProposalProcedure (ShelleyLedgerEra ConwayEra), Either (ExecutionUnits -> ScriptWitness WitCtxStake ConwayEra) (ScriptWitness WitCtxStake ConwayEra))]
            scriptProcedures = sequence $  mapMaybe makeWitness proposals

        in
        inEonForEra (Left $ FrameworkError FeatureNotSupported "Proposals are not supported in Babbage era")
              (\conwayOnward -> do
                scProcedures <- scriptProcedures

                let nonScriptProcedures =  OSet.fromSet (Set.fromList unProcedures)
                    (unresolved,resoved)  =  partitionEithers $ map (\(a,b)->case b of
                        Left e -> Left (a,e)
                        Right v -> Right (a,v)
                        ) scProcedures
                    revolveWihExUnits :: Ord a => Map a t -> Either FrameworkError t -> (a, t -> b) -> Either FrameworkError (a, b)
                    revolveWihExUnits euMap val (pp,partial) =
                        case Map.lookup  pp  euMap of
                          Nothing -> do
                            result <- val
                            pure  (pp,partial result)
                          Just eu -> pure  (pp,partial eu)

                    -- resolveAll  eu = sequence $ map (revolveWihExUnits eu) unresolved

                    resolvedMap :: Map (ProposalProcedure (ShelleyLedgerEra ConwayEra)) (ScriptWitness WitCtxStake ConwayEra)
                    resolvedMap = Map.fromList resoved
                if null unresolved
                  then
                    if null resoved  && null nonScriptProcedures
                      then Right (False,totalDeposit,\_ _-> pure Nothing)
                      else
                        Right $ (length resolvedMap > 0 || length nonScriptProcedures > 0, totalDeposit,\_ _-> do
                          pure $  Just$
                            Featured conwayOnward
                              (TxProposalProcedures nonScriptProcedures (BuildTxWith resolvedMap)))
                  else
                      Right $ (True, totalDeposit,\eu val   -> do
                        exUnitApplied <- mapM (revolveWihExUnits  eu val )  unresolved
                        Right $  Just$
                          Featured conwayOnward
                            (TxProposalProcedures nonScriptProcedures (BuildTxWith (resolvedMap <>(Map.fromList exUnitApplied) ))) )
              )

              (toCardanoEra conOnward)
      _ -> pure $ (False, nullValue, \_ _-> pure Nothing)
  -- where
  --   nonGuardrailScripts = filter (\TxProposa -> )
  where
    totalProposalDeposits :: (Ledger.EraPParams (ShelleyLedgerEra era)) => [TxProposal era]  -> Value
    totalProposalDeposits proposals = lovelaceToValue $ Coin $
        sum (map  txProposalDeposit proposals)
      where
      proposalDeposit (ProposalProcedure (Coin co) ra ga an) = co

      txProposalDeposit= \case
          TxProposal (ProposalProcedureModal pp) -> proposalDeposit pp
          TxProposalScript (ProposalProcedureModal pp) _ _ -> proposalDeposit pp
          TxProposalScriptReference (ProposalProcedureModal pp) _ _ -> proposalDeposit pp

ledgerCredToPaymentKeyHash :: ShelleyBasedEra targetEra -> Credential r (EraCrypto (ShelleyLedgerEra targetEra)) -> Maybe (Hash PaymentKey)
ledgerCredToPaymentKeyHash sbera cred = case cred of
  KeyHashObj kh ->Just  $  fromLedgerKeyHash sbera kh
  _ -> Nothing


computeBody ::  (IsTxBuilderEra era) =>  BabbageEraOnwards era
      -> LedgerProtocolParameters era
      -> UTxO era
      ->( [TxIn] -> [TxOut CtxTx era]-> Coin->  Either FrameworkError (TxBodyContent BuildTx era))
      -> AddressInEra era
      -> Set (Hash PaymentKey)
      -> Value
      -> [(TxIn, TxOut CtxUTxO era)]
      -> [TxOutput (TxOut CtxTx era)]
      -> Coin
      -> Either
          FrameworkError (TxBody era, Set (Hash PaymentKey), Coin)
computeBody beraOnward cpParam@(LedgerProtocolParameters lpparam) utxo bodyContentf changeAddr signatories   fixedInputSum availableInputs  fixedOutputs  fee = do
  -- Debug.traceM $ "ComputeBody:" 
  --            ++  "\n mintValue: " ++ show txMintValue'
  --            ++  "\n fee: " ++ show fee
  let mEraOnward=babbageEraOnwardsToMaryEraOnwards beraOnward
      mkChangeUtxo q= TxOut changeAddr  ( valueToRequiredEra bCardanoEra q) TxOutDatumNone ReferenceScriptNone
  changeTxOut  <-case findChange fixedOutputs of
    Nothing -> do
      pure $ mkChangeUtxo zeroValue
    Just to -> pure to

  (extraUtxos,change) <- selectUtxosConsideringChange mEraOnward (txoutMinLovelace lpparam ) (toCtxUTxOTxOut  changeTxOut) availableInputs startingChange
  -- Debug.traceM $ " change: " ++ show change
  -- Debug.traceM $ " Utxos : " ++ BS8.unpack (prettyPrintJSON extraUtxos )
  let
    maxChange = utxoListSum availableInputs <> startingChange
    missing = filterNegativeQuantity maxChange

    (feeUsed,changeUsed,outputs) = updateOutputs  mEraOnward  fee change fixedOutputs
    bodyContent allOutputs = -- Debug.trace (" Outs: " ++ BS8.unpack (prettyPrintJSON allOutputs))
    -- mkBodyContent pParam meta fixedInputs extraUtxos allOutputs collaterals txMintValue' fee
      bodyContentf   (map fst extraUtxos) allOutputs fee
    requiredSignatories = foldl (\acc (_,TxOut a _ _ _) -> fromMaybe acc (addressInEraToPaymentKeyHash a <&> flip Set.insert acc)) signatories  extraUtxos
    signatureCount=fromIntegral $ length requiredSignatories

  bc <- if changeUsed
          then  bodyContent outputs
          else do
              bodyContent (outputs++ [mkChangeUtxo change])

  case createAndValidateTransactionBody  shelleyBasedEra bc of
      Left tbe ->Left  $ FrameworkError  LibraryError  (show tbe)
      Right tb -> do
        let evaluateTransactionFee1=calculateMinTxFee (babbageEraOnwardsToShelleyBasedEra beraOnward) lpparam utxo tb signatureCount
        pure (tb,requiredSignatories, evaluateTransactionFee1 )

  where
    unSMaybe (SJust v) d =v
    unSMaybe SNothing d = d
    toLedgerTx :: TxBody era -> Ledger.Tx (ShelleyLedgerEra era)
    toLedgerTx tb = case makeSignedTransaction [] tb of
      ShelleyTx _ tx -> tx
    fixedOutputSum = foldMap txOutputVal fixedOutputs
      where
      txOutputVal :: ParsedOutput era -> Value
      txOutputVal (TxOutput (TxOut _ (TxOutValueShelleyBased era v) _ _) _  _ _) = fromLedgerValue era v
      txOutputVal (TxOutput (TxOut _ (TxOutValueByron a) _ _) _  _ _) = lovelaceToValue a

    startingChange=   fixedInputSum <>   negateValue (fixedOutputSum<> (if _hasFeeUtxo then mempty else lovelaceToValue fee ) )
    _hasFeeUtxo = any (\(TxOutput _ f _ _)->f) fixedOutputs


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
    selectUtxosConsideringChange :: (IsTxBuilderEra era)=> MaryEraOnwards era-> (TxOut ctx era -> Coin)
      -> TxOut ctx era
      -> [(TxIn, TxOut ctx era)]
      -> Value
      -> Either FrameworkError ([(TxIn, TxOut ctx era)], Value)
    selectUtxosConsideringChange meraOnward f txout  u c  = minimizeConsideringChange meraOnward txout f u (c <> utxoListSum u)

    minimizeConsideringChange :: (IsTxBuilderEra era)=> MaryEraOnwards era ->  TxOut ctx era
      -> (TxOut ctx era -> Coin)
      -> [(TxIn, TxOut ctx era)]
      -> Value
      -> Either FrameworkError ([(TxIn, TxOut ctx era)], Value)
    minimizeConsideringChange meraOnward txout f available change= case filterNegativeQuantity change of
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
                                              TxOutValueByron lo -> TxOut addr (valueToRequiredEra bCardanoEra (lovelaceToValue lo <> c)) md ReferenceScriptNone
                                              TxOutValueShelleyBased sbe va -> TxOut addr (valueToRequiredEra bCardanoEra (fromLedgerValue sbe va <> c)) md ReferenceScriptNone}
        txOutValue_ txout= case txout of { TxOut aie tov tod _-> txOutValueToValue tov }

    findChange :: [ParsedOutput era] -> Maybe (TxOut CtxTx era )
    findChange ous =   find (\(TxOutput _ _ c _)  -> c ) ous <&> (\(TxOutput v _ _ _)-> v)
    updateOutputs :: (IsTxBuilderEra era)=> MaryEraOnwards era
      -> Coin
      -> Value
      -> [ParsedOutput era]
      -> (BoolFee, BoolChange, [TxOut CtxTx era])
    updateOutputs  meraOnward fee change outputs' = updateOutput meraOnward False False  fee change outputs'

    updateOutput :: (IsTxBuilderEra era)=> MaryEraOnwards era -> BoolFee -> BoolChange ->  Coin -> Value -> [ParsedOutput era] ->  (BoolFee,BoolChange,[TxOut CtxTx era])
    updateOutput  _ _ _ _ _ []  =  (False,False,[])
    updateOutput  meraOnward _fUsed _cUsed  (fee) change (txOutput:outs) =let
        (feeUsed,changeUsed,result) = transformOut _fUsed _cUsed txOutput
        (feeUsed2,changeUsed2,others) = updateOutput meraOnward feeUsed changeUsed  (fee) change outs
        updatedOutput = (feeUsed  || feeUsed2 , changeUsed || changeUsed2, result : others )
        in   updatedOutput
      where
        transformOut feeUsed changeUsed  (TxOutput  tout@(TxOut aie v@(TxOutValueShelleyBased sbe va) ha sref) addFee addChange minAdaAction)=
            (feeUsed',changeUsed',modifiedTxOut)
          where
            modifiedTxOut = TxOut aie (valueToRequiredEra bCardanoEra changeNFeeIncluded) ha sref
            (feeUsed',feeIncluded) = includeFee (fromLedgerValue sbe va)
            (changeUsed', changeNFeeIncluded) = includeChange feeIncluded

            -- deduct fee from the val if needed
            includeFee val
              | feeUsed = (True, val)
              | addFee = (True, valueFromList [(AdaAssetId ,Quantity (- (unCoin fee)))] <> val)
              | otherwise = (False,val)

            -- add change to the val if needed
            includeChange val
              | changeUsed = (True,val)
              | addChange = (True,change<> val)
              | otherwise = (False,val)
        transformOut _ _ _ = error "UnExpected condition"


zeroValue :: Value
zeroValue = valueFromList [(AdaAssetId,0)]
nullValue :: Value
nullValue = valueFromList []


