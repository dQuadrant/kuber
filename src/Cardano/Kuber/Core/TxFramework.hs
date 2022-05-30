{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Cardano.Kuber.Core.TxFramework where


import Cardano.Api hiding ( PaymentCredential)
import Cardano.Api.Shelley hiding (PaymentCredential)
import Cardano.Kuber.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
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
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript, fromBuiltin)
import Cardano.Kuber.Core.TxBuilder
import Cardano.Kuber.Core.ChainInfo (DetailedChainInfo (DetailedChainInfo, dciConn), ChainInfo (getNetworkId, getConnectInfo, withDetails))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Cardano.Api.Crypto.Ed25519Bip32 (xPrvFromBytes)
import Debug.Trace (trace, traceM)
import qualified Data.Aeson as A
import Cardano.Ledger.Shelley.UTxO (txins)
import GHC.Num (wordToInteger)
import qualified Data.Map.Strict as StrictMap
import qualified Debug.Trace as Debug
import Data.Aeson (ToJSON(toJSON))
import qualified Data.Text as T
import Data.Text.Conversions (convertText)
import Prettyprinter.Extras (pretty)
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

type BoolChange   = Bool
type BoolFee = Bool
type  ParsedInput   = Either (Witness WitCtxTxIn AlonzoEra,TxOut CtxUTxO AlonzoEra) (Maybe ExecutionUnits,ScriptWitness WitCtxTxIn AlonzoEra,TxOut CtxUTxO  AlonzoEra)
type  ParsedOutput  = (BoolFee,BoolChange,TxOut CtxTx AlonzoEra)



-- Given TxBuilder object, Construct a txBody 
-- This IO code, constructs detailedChainInfo(protocolParam,costPerWord,eraHistory,SystemHistory) 
-- then queries required utxos used in inputs and calls  txBuilderToTxBody
txBuilderToTxBodyIO ::  ChainInfo i =>  i ->  TxBuilder  -> IO (Either FrameworkError  (TxBody AlonzoEra))
txBuilderToTxBodyIO  a b  = txBuilderToTxBodyIO'  a b <&> (<&> fst)

txBuilderToTxBody ::DetailedChainInfo ->  UTxO AlonzoEra -> TxBuilder   -> Either FrameworkError  (TxBody AlonzoEra )
txBuilderToTxBody   a b c  =  txBuilderToTxBody' a b c <&> fst

txBuilderToTx::DetailedChainInfo ->  UTxO AlonzoEra -> TxBuilder   -> Either FrameworkError  (Tx AlonzoEra)
txBuilderToTx a b c = txBuilderToTxBody'  a b c <&> snd

txBuilderToTxIO :: ChainInfo i => i -> TxBuilder -> IO (Either FrameworkError (Tx AlonzoEra))
txBuilderToTxIO a b   = txBuilderToTxBodyIO' a b   <&> ( <&> snd)


txBuilderToTxBodyIO'::  ChainInfo i =>  i ->  TxBuilder  -> IO (Either FrameworkError  (TxBody AlonzoEra,Tx AlonzoEra))
txBuilderToTxBodyIO' cInfo builder = do
  -- first determine the addresses and txins that need to be queried for value and address.
  let (selectionAddrs,sel_txins,sel_utxo) = mergeSelections
      (input_txins,input_utxo) = mergeInputs
      (txins,utxo) = ( sel_txins  <> input_txins <> collateralins, sel_utxo <> input_utxo <> collateralUtxo)
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
      _ -> Nothing

    mergeInputs = foldl  getInputTxins  (Set.empty,Map.empty) (txInputs  builder)
    getInputTxins :: (Set TxIn,Map TxIn (TxOut CtxUTxO AlonzoEra)) -> TxInput -> (Set TxIn,Map TxIn (TxOut CtxUTxO AlonzoEra))
    getInputTxins v@(ins,utxo) input = case input of
      TxInputResolved tir -> case tir of
        TxInputUtxo (UTxO uto) -> (ins, utxo <> uto)
        TxInputScriptUtxo tvs sd sd' m_eu (UTxO uto) -> (ins,utxo<>uto)
        TxInputScriptUtxoInlineDatum tvs  sd' m_eu (UTxO uto) -> (ins,utxo<>uto)
      TxInputUnResolved tiur -> case tiur of
        TxInputTxin ti -> (Set.insert ti ins,utxo)
        TxInputAddr aie -> v
        TxInputScriptTxin tvs sd sd' m_eu ti -> (Set.insert ti ins, utxo)
        TxInputScriptTxinInlineDatum  tvs  sd' m_eu  ti -> (Set.insert ti ins, utxo)

    mergeColaterals :: (Set TxIn,Map TxIn (TxOut CtxUTxO AlonzoEra) )
    mergeColaterals  =foldl (\(s,m) collateral -> case collateral of
                    TxCollateralTxin ti -> (Set.insert ti s,m)
                    TxCollateralUtxo (UTxO uto) -> (s,uto <> m) ) (mempty,mempty) (txCollaterals builder)

    mergeSelection :: ( Set AddressAny,Set TxIn, Map TxIn (TxOut CtxUTxO AlonzoEra))  -> TxInputSelection  -> (Set AddressAny,Set TxIn, Map TxIn (TxOut CtxUTxO AlonzoEra))
    mergeSelection (a,i,u) sel = case sel of
        TxSelectableAddresses aies -> (Set.union a  (Set.fromList $ map addressInEraToAddressAny aies),i,u)
        TxSelectableUtxos (UTxO uto) -> (a,i, uto <> u)
        TxSelectableTxIn tis -> (a,Set.union i (Set.fromList tis),u)
        TxSelectableSkey skeys -> (Set.union a (Set.fromList $ map (\s ->  toAddressAny $ skeyToAddr s (getNetworkId   cInfo ) ) skeys), i , u )

-- Construct TxBody from TxBuilder specification.
-- Utxos map must be provided for the utxos that are available in wallet and used in input
txBuilderToTxBody'::DetailedChainInfo ->  UTxO AlonzoEra -> TxBuilder   -> Either FrameworkError  (TxBody AlonzoEra,Tx AlonzoEra )
txBuilderToTxBody'  dCinfo@(DetailedChainInfo cpw conn pParam systemStart eraHistory ) (UTxO availableUtxo) (TxBuilder selections _inputs _outputs _collaterals validityStart validityEnd mintData extraSignatures explicitFee mChangeAddr metadata ) = do
  let network = getNetworkId  dCinfo
  meta<- if null metadata
          then  Right TxMetadataNone
          else  do
            case metadataFromJson TxMetadataJsonNoSchema (toJSON $ splitMetadataStrings  metadata) of
              Left tmje -> Left $ FrameworkError BadMetadata  (show tmje)
              Right tm -> Right $ TxMetadataInEra  TxMetadataInAlonzoEra tm
  resolvedInputs <- mapM resolveInputs _inputs
  fixedInputs <- usedInputs Map.empty (Right defaultExunits) resolvedInputs
  fixedOutputs <- mapM (parseOutputs network) _outputs
  collaterals <- if hasScriptInput
                  then  (case collaterals of
                    Nothing ->  Left $ FrameworkError BalancingError "No utxo available for collateral"
                    Just tis -> pure tis

                    )
                  else pure []
  let mintValue = foldMap (\(TxMintData _ _ value)->value) mintData
      witnessProvidedMap = Map.fromList $ map (\(TxMintData policyId sw _)->(policyId,sw)) mintData
      txMintValue' = TxMintValue MultiAssetInAlonzoEra mintValue $ BuildTxWith witnessProvidedMap
      fixedInputSum =  usedInputSum fixedInputs <> mintValue
      fee= Lovelace 100_000
      availableInputs = sortUtxos $ UTxO  $ Map.filterWithKey (\ tin _ -> Map.notMember tin fixedInputs) availableUtxo
      calculator= computeBody meta (Lovelace cpw) compulsarySignatories  fixedInputSum availableInputs (map fst collaterals) fixedOutputs
      colalteralSignatories = Set.fromList ( map snd collaterals)
      compulsarySignatories = foldl (\acc x -> case x of
                          Left (_,TxOut a _ _) -> case addressInEraToPaymentKeyHash  a of
                                                    Nothing -> acc
                                                    Just pkh -> Set.insert pkh acc
                          Right _ -> acc ) (appendExtraSignatures extraSignatures colalteralSignatories)   $ Map.elems  fixedInputs
  (txBody1,signatories,fee1) <-  calculator  fixedInputs txMintValue'  fee
  if  not requiresExUnitCalculation
    then  ( do
      (body2,signatories2,fee2) <- calculator fixedInputs txMintValue' fee1
      if fee1 /= fee2  then Left $ FrameworkError LibraryError "Transaction not balanced even in 3rd iteration" else pure  ()
      pure (body2, makeSignedTransaction [] body2)
    )
    else (
          let evaluateBodyWithExunits body fee= do
                        exUnits <- evaluateExUnitMap dCinfo ( UTxO availableUtxo) body
                        inputs' <- usedInputs  (Map.map Right exUnits ) (Right defaultExunits)  resolvedInputs
                        calculator inputs' txMintValue' fee
          in do
            (txBody2,signatories2,fee2) <- evaluateBodyWithExunits  txBody1 fee1
            (txBody3,signatories3,fee3) <- evaluateBodyWithExunits  txBody2 fee2
            (txBody4,signatories4,fee4) <- evaluateBodyWithExunits  txBody3 fee3

            if fee4==fee3
              then pure (txBody4,makeSignedTransaction [] txBody4)
              else evaluateBodyWithExunits txBody4 fee4 <&> (\(txBody5,signatories5,_)-> respond txBody5 signatories5)
      )

  where
    respond txBody signatories = (txBody,makeSignedTransaction (map (toWitness txBody) $ mapMaybe (`Map.lookup` availableSkeys) $ Set.toList signatories) txBody)
    toWitness body skey = makeShelleyKeyWitness body (WitnessPaymentKey skey)

    availableSkeys =  Map.fromList $  map (\x -> (skeyToPaymentKeyHash x, x)) $  concat (mapMaybe (\s -> case s of
        TxSelectableSkey sks -> Just sks
        _ -> Nothing )  selections) ++ mapMaybe (\x -> case x of
      TxSignatureSkey sk -> Just sk
      _ -> Nothing) extraSignatures

    mapPolicyIdAndWitness :: TxMintData -> (PolicyId, ScriptWitness WitCtxMint AlonzoEra)
    mapPolicyIdAndWitness (TxMintData pId sw _)= (pId, sw)

    hasScriptInput = any (\case
      TxInputResolved TxInputScriptUtxo {} -> True
      TxInputUnResolved TxInputScriptTxin {} -> True
      _ -> False ) _inputs

    requiresExUnitCalculation = any (\case
      TxInputResolved (TxInputScriptUtxo _ _ _ Nothing _)-> True
      TxInputUnResolved (TxInputScriptTxin _ _ _ Nothing _ ) -> True
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

    collaterals ::   Maybe [(TxIn,Hash PaymentKey )]
    collaterals   = case foldl getCollaterals [] _collaterals of
                          [] -> case mapMaybe canBeCollateral $ Map.toList availableUtxo of
                            [] -> Nothing
                            v -> let  (tin,pkh,_) =minimumBy sortingFunc v in Just [(tin,pkh)]
                          v-> Just v
        where
        canBeCollateral :: (TxIn  , TxOut ctx AlonzoEra) -> Maybe (TxIn, Hash PaymentKey, Integer)
        canBeCollateral v@(ti, to@(TxOut addr val mDatumHash)) = case mDatumHash of
                              TxOutDatumNone -> case val of
                                TxOutAdaOnly _ (Lovelace v) ->  addressInEraToPaymentKeyHash  addr >>= (\pkh -> Just (ti,pkh,v))
                                TxOutValue _ va ->  let _list = valueToList va
                                                    in if length _list == 1
                                                        then  case addressInEraToPaymentKeyHash  addr of
                                                                Nothing -> Nothing
                                                                Just pkh -> Just ( ti,pkh,case snd $ head _list of { Quantity n -> n } )
                                                        else Nothing
                              _ -> Nothing
        filterCollateral = mapMaybe  canBeCollateral $ Map.toList availableUtxo

        -- sort based on following conditions => Utxos having >4ada come eariler and the lesser ones come later.
        sortingFunc :: (TxIn,a,Integer) -> (TxIn,a,Integer)-> Ordering
        sortingFunc (_,_,v1) (_,_,v2)
          | v1 < 4 = if v2 < 4 then  v2 `compare` v1 else GT
          | v2 < 4 = LT
          | otherwise = v1 `compare` v2


    getCollaterals  accum  x = case x  of
        TxCollateralTxin txin -> accum++ (case Map.lookup txin availableUtxo of
          Nothing -> error "Collateral input missing in utxo map"
          Just (TxOut a v dh) -> case addressInEraToPaymentKeyHash  a of
                                    Just pkh ->  (txin,pkh) : accum
                                    Nothing -> error "Invalid address type utxo in collateral"
                                   )
        TxCollateralUtxo (UTxO mp) ->  accum ++ map (\(tin,TxOut a v dh) -> case addressInEraToPaymentKeyHash  a of
                                                                                 Just pkh -> (tin,pkh)
                                                                                 Nothing -> error "invalid address type utxo in collateral"
                      ) (Map.toList  mp)
    isJust (Just x)  = True
    isJust _ = False

    computeBody meta cpw signatories fixedInputSum availableInputs collaterals fixedOutputs fixedInputs txMintValue' fee = do
      changeTxOut <-case findChange fixedOutputs of
        Nothing -> do
          changeaddr <- monadFailChangeAddr
          pure (TxOut changeaddr  ( TxOutValue MultiAssetInAlonzoEra  (valueFromList [(AdaAssetId ,0)])) TxOutDatumNone)
        Just to -> pure to

      (extraUtxos,change) <- selectUtxosConsideringChange (calculateTxoutMinLovelaceWithcpw cpw) (toCtxUTxOTxOut  changeTxOut) availableInputs startingChange
      let
        maxChange = utxoListSum availableInputs <> startingChange
        missing = filterNegativeQuantity maxChange
        (feeUsed,changeUsed,outputs) = updateOutputs  fee change fixedOutputs
        bodyContent allOutputs = mkBodyContent meta fixedInputs extraUtxos allOutputs collaterals txMintValue' fee
        requiredSignatories = foldl (\acc (_,TxOut a _ _) -> fromMaybe acc (addressInEraToPaymentKeyHash  a <&> flip Set.insert acc)) signatories  extraUtxos
        signatureCount=fromIntegral $ length requiredSignatories
      bc <- if changeUsed
              then pure $ bodyContent outputs
              else do
                changeaddr <-  monadFailChangeAddr
                pure $ bodyContent (outputs++ [TxOut changeaddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumNone])
      case makeTransactionBody bc of
          Left tbe ->Left  $ FrameworkError  LibraryError  (show tbe)
          Right tb -> pure (tb,requiredSignatories,evaluateTransactionFee pParam tb signatureCount 0)

      where
        startingChange=   fixedInputSum <>   negateValue(fixedOutputSum<> if _hasFeeUtxo then mempty else lovelaceToValue fee )
        _hasFeeUtxo = any (\(a,b,c)->a) fixedOutputs


        monadFailChangeAddr= case mChangeAddr of
          Nothing ->  if null usableAddresses
                        then Left $ FrameworkError BalancingError "no change address"
                        else pure $ head usableAddresses

          Just aie -> pure aie
        usableAddresses :: [AddressInEra AlonzoEra]
        usableAddresses=concat $ mapMaybe findInput selections
        findInput :: TxInputSelection ->Maybe [AddressInEra AlonzoEra]
        findInput v= case v of
          TxSelectableAddresses aies -> Just aies
          TxSelectableUtxos (UTxO mp) -> Just $ map (\(TxOut aie tov tod) -> aie ) $ Map.elems mp
          TxSelectableTxIn tis -> Just $ foldl   (\addrs x -> case Map.lookup x availableUtxo of
                    Nothing -> addrs
                    Just (TxOut aie tov tod) -> aie: addrs) [] tis
          TxSelectableSkey sk -> Just $ foldl (\addrs sk -> addrs ++ [skeyToAddrInEra sk (getNetworkId dCinfo)]) [] sk
    getTxin :: Map TxIn ParsedInput -> [(TxIn,TxOut CtxUTxO AlonzoEra )]-> [(TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra ))]
    getTxin v  v2 = map ( uncurry totxIn)  (Map.toList v) ++ map toPubKeyTxin v2

    toPubKeyTxin :: (TxIn,a) -> (TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra ))
    toPubKeyTxin (v1,v2) =(v1,BuildTxWith $ KeyWitness KeyWitnessForSpending )


    totxIn :: TxIn ->  ParsedInput -> (TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra ))
    totxIn  i  parsedInput = case parsedInput of
      Left (a,b) -> (i,BuildTxWith a) 
      Right (e,a,b) -> (i,BuildTxWith  ( ScriptWitness ScriptWitnessForSpending a )  )
    mkBodyContent meta fixedInputs extraUtxos outs collateral  txMintValue' fee =
      (TxBodyContent {
        txIns= getTxin fixedInputs extraUtxos ,
        txInsCollateral= if null collateral then TxInsCollateralNone  else TxInsCollateral CollateralInAlonzoEra collateral,
        txOuts=outs,
        Cardano.Api.Shelley.txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra  fee,
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
                    else TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra $
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

    fixedOutputSum = foldMap txOutputVal _outputs
      where
      txOutputVal :: TxOutput -> Value
      txOutputVal o = case o of { TxOutput toc b b' -> case toc of
                                    TxOutAddress aie va -> va
                                    TxOutScriptAddress aie va ha -> va
                                    TxOutPkh pkh va -> va
                                    TxOutScript tvs va ha -> va
                                    TxOutScriptAddressWithData _ va _ -> va
                                    TxOutScriptWithData _ va _ -> va
                                }
    zeroValue = valueFromList []
    findChange :: [ParsedOutput] -> Maybe (TxOut CtxTx AlonzoEra )
    findChange ous =   find (\(_,c,v) -> c ) ous <&> (\(_,_,v)-> v)
    updateOutputs  fee change outputs' = updateOutput False False (getNetworkId  dCinfo) fee change outputs'
    updateOutput :: BoolFee -> BoolChange -> NetworkId -> Lovelace -> Value -> [ParsedOutput] ->  (BoolFee,BoolChange,[TxOut CtxTx AlonzoEra])
    updateOutput _ _ _ _ _ []  =  (False,False,[])
    updateOutput _fUsed _cUsed network (Lovelace fee) change (txOutput:outs) =let
        (feeUsed,changeUsed,result) = transformOut _fUsed _cUsed txOutput
        (feeUsed2,changeUsed2,others) = updateOutput feeUsed changeUsed network (Lovelace fee) change outs
        updatedOutput = (feeUsed  || feeUsed2 , changeUsed || changeUsed2, result : others )
        in   updatedOutput
      where
        transformOut feeUsed changeUsed  (addFee,addChange,tout@(TxOut aie v@(TxOutValue _ va) ha))=
            (feeUsed',changeUsed',modifiedTxOut)
          where
            modifiedTxOut = TxOut aie (TxOutValue MultiAssetInAlonzoEra changeNFeeIncluded) ha
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


    parseOutputs ::  NetworkId -> TxOutput -> Either FrameworkError   ParsedOutput
    parseOutputs  networkId output = case output of { TxOutput toc b b' -> case toc of
                                              TxOutAddress aie va -> pure  (b,b',TxOut aie  (TxOutValue MultiAssetInAlonzoEra va ) TxOutDatumNone )
                                              TxOutScriptAddress aie va ha -> pure (b,b',TxOut aie (TxOutValue MultiAssetInAlonzoEra va) (TxOutDatumHash ScriptDataInAlonzoEra ha ))
                                              TxOutScriptAddressWithData aie va sd -> pure (b,b',TxOut aie (TxOutValue MultiAssetInAlonzoEra va ) (TxOutDatum ScriptDataInAlonzoEra  sd))
                                              TxOutPkh pkh va -> case pkhToMaybeAddr (getNetworkId  dCinfo) pkh of
                                                      Nothing -> Left  $ FrameworkError ParserError  ("Cannot convert PubKeyHash to Address : "++ show pkh)
                                                      Just aie ->  pure  (b,b',TxOut aie  (TxOutValue MultiAssetInAlonzoEra va ) TxOutDatumNone )
                                              TxOutScript (TxValidatorScript (ScriptInAnyLang lang script)) va ha ->
                                                let payCred = PaymentCredentialByScript (hashScript script)
                                                    addr = makeShelleyAddress networkId payCred NoStakeAddress
                                                    addrInEra = AddressInEra (ShelleyAddressInEra ShelleyBasedEraAlonzo) addr
                                                in pure (b,b',TxOut addrInEra (TxOutValue MultiAssetInAlonzoEra va) (TxOutDatumHash ScriptDataInAlonzoEra ha ))
                                              TxOutScriptWithData  (TxValidatorScript (ScriptInAnyLang lang script)) va sd ->
                                                let payCred = PaymentCredentialByScript (hashScript script)
                                                    addr = makeShelleyAddress networkId payCred NoStakeAddress
                                                    addrInEra = AddressInEra (ShelleyAddressInEra ShelleyBasedEraAlonzo) addr
                                                in pure (b,b',TxOut addrInEra (TxOutValue MultiAssetInAlonzoEra va) (TxOutDatum ScriptDataInAlonzoEra  sd))}

    resolveInputs ::  TxInput -> Either FrameworkError    TxInputResolved_
    resolveInputs v = case v of
      TxInputResolved tir -> pure tir
      TxInputUnResolved (TxInputTxin txin) ->  doLookup txin <&> TxInputUtxo
      TxInputUnResolved (TxInputAddr addr) ->   filterAddrUtxo addr <&> TxInputUtxo
      TxInputUnResolved (TxInputScriptTxin s d r exunit txin) -> doLookup txin <&>  TxInputScriptUtxo s d r exunit
      TxInputUnResolved (TxInputScriptTxinInlineDatum s  r exunit txin) -> doLookup txin <&>  TxInputScriptUtxoInlineDatum s r exunit

      where
        filterAddrUtxo addr =pure $ UTxO $ Map.filter (ofAddress addr) availableUtxo
        ofAddress addr (TxOut a _ _)= addr == a
        doLookup tin = case Map.lookup tin availableUtxo of
          Nothing -> Left $ FrameworkError LibraryError  "Input Utxo missing in utxo map"
          Just to ->pure $ UTxO $ Map.singleton  tin  to
    mapInputs  exlookup onMissing is=do
          tuples<- mapM (toInput exlookup onMissing) is
          pure $ Map.fromList $ concat tuples
    toInput ::  Map TxIn (Either ScriptExecutionError ExecutionUnits)  -> Either FrameworkError ExecutionUnits-> TxInputResolved_  -> Either FrameworkError   [(TxIn,ParsedInput)]
    toInput exUnitLookup onMissing inCtx = case inCtx of
      TxInputUtxo (UTxO txin) ->  pure $ map (\(_in,val) -> (_in,Left (  KeyWitness KeyWitnessForSpending, val) ))  $ Map.toList txin
      TxInputScriptUtxo (TxValidatorScript s) d r mExunit (UTxO txin) ->mapM (\(_in,val) -> do
                                                                exUnit <- getExUnit _in mExunit
                                                                witness <-  createTxInScriptWitness s d r exUnit
                                                                pure (_in,Right (mExunit, witness,val )) ) $ Map.toList txin
      -- TODO change this to proper input tuxo in babbage era                                                          
      TxInputScriptUtxoInlineDatum (TxValidatorScript s)  r mExunit (UTxO txin) ->
                                                                Left $ FrameworkError EraMisMatch "Inline datum is not supported in alonzo era"
      
      where

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
            Left (_,TxOut  _ v _) -> txOutValueToValue v
            Right (_,_,TxOut  _ v _) -> txOutValueToValue v

      in foldMap inputValue $ Map.elems mp

    sortUtxos :: UTxO AlonzoEra ->  [(TxIn,TxOut CtxUTxO AlonzoEra )]
    sortUtxos  ( UTxO utxoMap) = sortBy sortingFunc ( Map.toList utxoMap)
        where
        -- sort the txouts based on following condition
        -- - the ones with multiple assets comes first
        -- - then the ones with lower lovelace amount come
        -- - then the ones with higher lovelace amount come
        sortingFunc :: (TxIn,TxOut CtxUTxO AlonzoEra) -> (TxIn,TxOut CtxUTxO AlonzoEra)-> Ordering
        sortingFunc (_,TxOut _ (TxOutAdaOnly _ v1) _) (_, TxOut _ (TxOutAdaOnly _ v2)  _) = v1 `compare` v2
        sortingFunc (_,TxOut _ (TxOutAdaOnly _ (Lovelace v))  _) (_, TxOut _ (TxOutValue _ v2) _) = LT
        sortingFunc (_,TxOut _ (TxOutValue _ v1) _) (_, TxOut _ (TxOutAdaOnly _ v2) _) =  GT
        sortingFunc (_,TxOut _ (TxOutValue _ v1) _) (_, TxOut _ (TxOutValue _ v2) _) =  let l1= length ( valueToList v1)
                                                                                            l2= length (valueToList v2) in
                                                                                        if l1==l2
                                                                                        then selectAsset v1 AdaAssetId `compare` selectAsset v2  AdaAssetId
                                                                                        else l2 `compare` l1

    -- from the utxos, try to remove utxos that can be removed while keeping the change positive or zero if possible
    selectUtxos u c = minimizeUtxos u (c <> utxoListSum u)
    minimizeUtxos utxos remainingChange= case utxos of
      []     -> ([] ,remainingChange)
      (txIn,txOut@(TxOut _ txOutVal _)):subUtxos -> if val `valueLte` remainingChange
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
                      (case Foldable.find (\(tin,utxo) -> extraLove utxo > (minLove- existingLove)) unmatched of
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
            Lovelace l -> l
        -- extra lovelace in this txout over the txoutMinLovelace
        extraLove utxo  = selectLove - minLoveInThisTxout
            where
              minLoveInThisTxout=case  f $ txoutWithChange val of
                  Lovelace l -> l
              val= txOutValue_ utxo
              selectLove = case selectAsset val AdaAssetId of { Quantity n -> n }
        txoutWithChange c = case txout of { TxOut addr v md-> case v of
                                              TxOutAdaOnly oasie lo -> TxOut addr (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue lo <> c)) md
                                              TxOutValue masie va -> TxOut addr (TxOutValue MultiAssetInAlonzoEra (va <> c)) md }

        txOutValue_ txout= case txout of { TxOut aie tov tod -> txOutValueToValue tov }
    txLowerBound = case validityStart of
                                Nothing -> TxValidityNoLowerBound
                                Just v -> TxValidityLowerBound ValidityLowerBoundInAlonzoEra   (toSlot v)
    txUpperBound = case validityEnd of
      Nothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
      Just n -> TxValidityUpperBound ValidityUpperBoundInAlonzoEra (toSlot n)
    plutusWitness script _data redeemer exUnits = PlutusScriptWitness PlutusScriptV2InAlonzo
                            PlutusScriptV2
                            script
                            (ScriptDatumForTxIn _data) -- script data
                            redeemer -- script redeemer
                            exUnits
    defaultExunits=ExecutionUnits {executionSteps= 6000000000, executionMemory=14000000}
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
    toSlot tStamp= case getNetworkId  dCinfo of
      Mainnet -> SlotNo $ fromIntegral $  mainnetSlot tStamp
      Testnet nm -> SlotNo $ fromIntegral $ testnetSlot tStamp
    testnetSlot timestamp= ((timestamp -1607199617000) `div` 1000 )+ 12830401 -- using epoch 100 as refrence
    mainnetSlot timestamp = ((timestamp -1596491091000 ) `div` 1000 )+ 4924800 -- using epoch 209 as reference

-- mkBalancedBody :: ProtocolParameters
--   -> UTxO AlonzoEra
--   -> TxBodyContent BuildTx AlonzoEra
--   -> Value
--   -> AddressInEra AlonzoEra
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
--       minLove = case  f $ TxOut walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumNone of
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
--       performMinCalculation addr val hash =TxOut  addr (TxOutValue MultiAssetInAlonzoEra  (val <> lovelaceToValue minLovelace)) hash
--         where
--          minLovelace = minval addr (val <> lovelaceToValue (Lovelace 1_000_000)) hash

--       minval add v hash= calculator (TxOut add (TxOutValue MultiAssetInAlonzoEra v) hash )

--   modifiedBody initialOuts txins change fee= content
--     where

--       content=(TxBodyContent  {
--             txIns= reorderInputs$ txins ++ txIns txbody,
--             txInsCollateral=txInsCollateral txbody,
--             txOuts=  if nullValue change
--                   then initialOuts
--                   else initialOuts ++ [ TxOut  walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumNone]  ,
--             txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra  fee,
--             -- txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
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




-- mkTxExplicitFee ::DetailedChainInfo -> TxBuilder -> TxBody AlonzoEra
-- mkTxExplicitFee = error "sad"

-- gatherInfo :: ChainInfo i -> i  -> TxBuilder  ->  IO (Either AcquireFailure TxContext)
-- gatherInfo cInfo  txBuilder@TxBuilder{txSelections, txInputs} = do 
--   error "sad"
--   where