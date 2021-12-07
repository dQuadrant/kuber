{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Cardano.Contrib.Easy.TxFramework
where


import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Contrib.Easy.Error
import Ledger (PubKeyHash)
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Contrib.Easy.Util
import Data.Functor ((<&>))
import qualified Ledger as Plutus
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Ledger.Scripts (Validator)
import Codec.Serialise (serialise)
import Cardano.Contrib.Easy.Context
import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate)
import qualified Data.Foldable as Foldable

data TxCtxInput  = UtxoCtxIn (UTxO AlonzoEra) | TxInCtxIn TxIn | ScriptCtxTxIn(PlutusScript PlutusScriptV1,ScriptData,ScriptData,TxIn) | ScriptUtxoCtxTxIn(PlutusScript PlutusScriptV1,ScriptData,ScriptData,UTxO AlonzoEra)deriving (Show)
data TxCtxOutput =
    AddrCtxOut (AddressInEra AlonzoEra,Value)
  | PkhCtxOut (PubKeyHash,Value)
  | ScriptCtxOut (PlutusScript PlutusScriptV1,Value, Hash ScriptData)  deriving (Show)

-- select a utxo and add change to it.
data TxCtxChange = TxCtxChangeUnset | TxCtxChange TxCtxOutput | TxCtxChangeAddr (AddressInEra AlonzoEra) deriving (Show)
-- Context Builder for test transaction
-- You will not use this directly, instead use builder functions
-- to compose this structure.
data TxOperationBuilder=TxOperationBuilder{
    ctxChange:: TxCtxChange,
    ctxInputs:: [TxCtxInput], -- inputs in this transaction
    ctxOutputs::[TxCtxOutput], -- outputs in this transaction
    ctxSignatures :: [SigningKey PaymentKey] -- public key signatures in this transaction
  } deriving(Show)



instance Semigroup TxOperationBuilder where
  (<>) ctx1 ctx2=TxOperationBuilder{
          ctxChange= case ctxChange ctx2 of
            TxCtxChangeUnset -> ctxChange ctx1
            _  -> ctxChange ctx2 ,
    ctxInputs=ctxInputs ctx1 ++ ctxInputs ctx2,
    ctxOutputs=ctxOutputs ctx1 ++ ctxOutputs ctx2,
    ctxSignatures=ctxSignatures ctx1 ++ctxSignatures ctx2
  }

txUseForChange  operation = if null (ctxOutputs operation)
  then TxOperationBuilder (ctxChange operation) [] [] []
  else TxOperationBuilder (TxCtxChange $ head $ ctxOutputs operation) [] [] []

instance Monoid TxOperationBuilder where
  mempty = TxOperationBuilder TxCtxChangeUnset [] [] []
-- mkTxWithWallet :: SigningKey PaymentKey -> TxOperationBuilder -> Ledger.Tx

ctxInput v =  TxOperationBuilder TxCtxChangeUnset [v] [] []
ctxOutput v = TxOperationBuilder TxCtxChangeUnset [] [v] []
ctxSignature :: SigningKey PaymentKey -> TxOperationBuilder
ctxSignature v = TxOperationBuilder TxCtxChangeUnset [] [] [v]
-- In this transaction, pay some value to the wallet.
-- It will be included in the tx_out of this transaction
txPayTo:: AddressInEra AlonzoEra ->Value ->TxOperationBuilder
txPayTo addr v= ctxOutput $ AddrCtxOut  (addr, v)

txConsumeUtxos :: UTxO AlonzoEra -> TxOperationBuilder
txConsumeUtxos utxo =  ctxInput $ UtxoCtxIn  utxo


txPayToPkh:: PubKeyHash  ->Value ->TxOperationBuilder
txPayToPkh pkh v= ctxOutput $   PkhCtxOut  (pkh, v)
-- In this transaction send  x lovelace to an wallet.
-- It will appear in tx_out
txPayLovelaceTo :: AddressInEra AlonzoEra  -> Integer -> TxOperationBuilder
txPayLovelaceTo w v=txPayTo w (lovelaceToValue $quantityToLovelace $ Quantity v)


-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.
txPayToScript:: ToData _data=>PlutusScript PlutusScriptV1 -> _data -> Value->TxOperationBuilder
txPayToScript script _data v = ctxOutput $ ScriptCtxOut (script,v,hashScriptData  $ dataToScriptData   _data)

-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.
txPayToValidator:: ToData _data=>Validator  -> _data -> Value->TxOperationBuilder
txPayToValidator validator _data v = ctxOutput $  ScriptCtxOut (PlutusScriptSerialised serialisedScript ,v,hashScriptData  $ dataToScriptData   _data)
  where
    serialisedScript :: SBS.ShortByteString
    serialisedScript = SBS.toShort . LBS.toStrict $ serialise script
    script  = Plutus.unValidatorScript validator

-- Redeem from Script Address.
txRedeem:: (ToData _data,ToData redeemer)=>TxIn -> PlutusScript PlutusScriptV1 ->_data-> redeemer -> TxOperationBuilder
txRedeem txin script _data _redeemer = ctxInput $ ScriptCtxTxIn (script,dataToScriptData _data,dataToScriptData _redeemer,txin)

-- Redeem from Script Address.
txRedeemUtxo :: (ToData a2, ToData a3) =>
  UTxO AlonzoEra
  -> PlutusScript PlutusScriptV1 -> a2 -> a3 -> TxOperationBuilder
txRedeemUtxo utxo script _data _redeemer = ctxInput $ ScriptUtxoCtxTxIn (script,dataToScriptData _data,dataToScriptData _redeemer,utxo)

txAddSignature :: SigningKey PaymentKey -> TxOperationBuilder
txAddSignature = ctxSignature

-- Redeem from Script Address.
txRedeemUtxoWithValidator :: (ToData a2, ToData a3) =>
  UTxO AlonzoEra
  -> Validator -> a2 -> a3 -> TxOperationBuilder
txRedeemUtxoWithValidator utxo validator _data _redeemer =
    ctxInput $ ScriptUtxoCtxTxIn (PlutusScriptSerialised  serialisedScript ,dataToScriptData _data,dataToScriptData _redeemer,utxo)
  where
    serialisedScript :: SBS.ShortByteString
    serialisedScript = SBS.toShort . LBS.toStrict $ serialise script
    script  = Plutus.unValidatorScript validator

data TxResult=TxResult {
   txResultFee :: Lovelace ,
   txResultIns::[(TxIn, Cardano.Api.Shelley.TxOut AlonzoEra)],
   txResultBodyCotent::TxBodyContent BuildTx AlonzoEra,
   txResultBody:: TxBody AlonzoEra
}

mkTx ctx builder walletAddr =mkTxWithChange ctx builder walletAddr walletAddr

mkTxWithChange :: IsNetworkCtx v =>v ->TxOperationBuilder
  -> AddressInEra AlonzoEra
  ->  AddressInEra AlonzoEra
  -> IO TxResult
mkTxWithChange networkCtx (TxOperationBuilder change input output signature ) payerAddrInEra changeAddrInEra   = do
  (NetworkContext conn  pParam) <- toNetworkContext networkCtx
  walletAddr <-unMaybe (SomeError "unexpected error converting address to another type") (deserialiseAddress AsAddressAny (serialiseAddress  payerAddrInEra))
  UTxO walletUtxos <- queryUtxos conn walletAddr
  mappedOutput <- mapM (toOuotput (networkCtxNetwork networkCtx)) output
  (operationUtxos,_txins) <- resolveAndSumIns conn input
  let operationUtoSum=foldMap txOutValue (Map.elems operationUtxos)
  if not hasScriptInput
  then executeMkBalancedBody pParam  (UTxO walletUtxos) (mkBody  _txins mappedOutput TxInsCollateralNone  pParam)  operationUtoSum changeAddrInEra signatureCount

  else (do
    (FullNetworkContext _  _ systemStart eraHistory ) <- toFullNetworkContext networkCtx
    let possibleCollaterals= Map.filter isOnlyAdaTxOut walletUtxos
    if null possibleCollaterals then throw (SomeError "BuildTx: No utxo usable as collateral") else pure ()
    let (myCollateral,_)= head $ Map.toList possibleCollaterals
    let txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [myCollateral ]

    let bodyRevsion0 =   mkBody _txins mappedOutput txInsCollateral pParam
    _result<-case
        mkBalancedBody
          pParam (UTxO walletUtxos) bodyRevsion0 operationUtoSum
          changeAddrInEra (fromIntegral $ length signature + 1)
      of
        Left tbe -> throw $ SomeError $ "First Balance :" ++ show tbe
        Right x0 -> pure x0
    let (TxResult fee usedWalletUtxos balancedRevisionContent0 balancedRevision0) = _result
    let usedUtxos=UTxO (Map.fromList usedWalletUtxos <> operationUtxos)
    let (orderedIns,orderedOuts)=case balancedRevision0 of {
          ShelleyTxBody sbe
          (LedgerBody.TxBody ins _ outs _ _ _ _ _ _ _ _ _ _) scs tbsd m_ad tsv
          -> (map fromShelleyTxIn  $ Set.toList ins,outs)
    }
    let eExunits= evaluateTransactionExecutionUnits AlonzoEraInCardanoMode systemStart eraHistory pParam usedUtxos balancedRevision0
    case eExunits of
      Left tvie ->do 
          putStrLn  "[WARNING] :: exUnit calculation failed. Using default"  -- throw $ SomeError $ "ExecutionUnitCalculation :" ++ show tvie
          pure $ TxResult fee usedWalletUtxos balancedRevisionContent0 balancedRevision0
      Right mp ->do 
        case applyTxInExecutionUnits _txins (txIns balancedRevisionContent0) orderedIns mp of
          Left se -> do 
            putStrLn $ "[WARNING] :: exUnit application failed. Using default :" ++ show se  -- throw $ SomeError $ "ExecutionUnitCalculation :" ++ show tvie
            pure $ TxResult fee usedWalletUtxos balancedRevisionContent0 balancedRevision0
          Right modifiedIns -> do 
            executeMkBalancedBody pParam (UTxO walletUtxos)  (mkBody  modifiedIns mappedOutput txInsCollateral pParam)  operationUtoSum changeAddrInEra signatureCount)

  where
    signatureCount = fromIntegral $ length signature + 1
    hasScriptInput = any (\case
                              ScriptCtxTxIn x0 -> True
                              ScriptUtxoCtxTxIn x0 -> True
                              _ -> False )  input
    applyTxInExecutionUnits ::
         [(TxIn,BuildTxWith  BuildTx (Witness WitCtxTxIn AlonzoEra))] 
      -> [(TxIn,BuildTxWith  BuildTx (Witness WitCtxTxIn AlonzoEra))]
      -> [TxIn]
      -> Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits)
      -> Either SomeError [(TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
    applyTxInExecutionUnits originalIns ins orderedIns execUnitMap = do
      resolvedIns <- mapM   doMap (zip [0..] orderedIns)
      pure $ filter (\v -> fst v   `elem` oldSet) resolvedIns
      where
        oldSet=Set.fromList $ map fst originalIns
        insMap=Map.fromList ins
        doMap  (index,txIn)= case Map.lookup txIn insMap of
          Nothing   ->  Left $  SomeError   "Look how they maccasacred my boy"
          Just item -> case Map.lookup (ScriptWitnessIndexTxIn index) execUnitMap of
            Nothing -> pure (txIn,item) -- this should never happen
            Just a -> case a of
              Left e -> Left $ SomeError  $ "ResolveExecutionUnit for txin: "++ show e
              Right exUnits ->Right $  case  item of { BuildTxWith wit -> case wit of
                                KeyWitness kwic -> throw $ SomeError  "ResolveExecutionUnit for txin: wrong hit "
                                ScriptWitness swic sw -> case sw of
                                  SimpleScriptWitness slie ssv ss -> throw $ SomeError   "ResolveExecutionUnit for txin: wrong hit "
                                  PlutusScriptWitness slie psv ps sd sd' eu ->
                                    (
                                        txIn
                                      , BuildTxWith $  ScriptWitness ScriptWitnessForSpending
                                          $ PlutusScriptWitness  slie psv ps sd sd' (ExecutionUnits {executionSteps=1794494645, executionMemory=5334093})
                                    )
                                    }
        transformIn (txIn,wit) exUnit= (txIn  ,case BuildTxWith $ KeyWitness KeyWitnessForSpending of {
          BuildTxWith wit' -> wit } )

    unEitherExecutionUnit e= case e of
      Left e -> throw $  SomeError  $ "EvaluateExecutionUnits: " ++ show e
      Right v -> pure v
    resolveAndSumIns conn inputs = if null unresolved
      then pure (Map.fromList $ map  fst  resolved,map snd resolved)
      else do
        (UTxO utos) <-queryTxins conn (map fst unresolved )
        if length utos /= length unresolved then throw (SomeError "QueryUtxoValue: Requested utxos not found") else pure ()
        let fixedTxins=map snd resolved ++ unresolved
        let usedUtxos=  Map.union  utos  (Map.fromList $map fst resolved)
        pure (usedUtxos,fixedTxins)
        where
          all= partitionEithers vs
          resolved=concat $ snd all
          unresolved=fst all
          vs=map toInput inputs
    txOutValue (TxOut _ v _) = case v of
      TxOutAdaOnly oasie lo -> lovelaceToValue lo
      TxOutValue masie va -> va

    toInput ( inCtx ::TxCtxInput)= case inCtx of
      UtxoCtxIn (UTxO mp) -> Right $  map (\item-> (item,(fst item,BuildTxWith $ KeyWitness KeyWitnessForSpending)))   (Map.toList mp)
      TxInCtxIn ti -> Left (ti,BuildTxWith $ KeyWitness KeyWitnessForSpending)
      ScriptCtxTxIn (script,_data,_redeemer,txin) -> Left (txin,BuildTxWith $  ScriptWitness ScriptWitnessForSpending $ plutusWitness script _data _redeemer defaultExunits)
      ScriptUtxoCtxTxIn (script, _data,_redeemer,UTxO mp) -> Right $ map (\item-> (item,(fst  item,BuildTxWith $  ScriptWitness ScriptWitnessForSpending $ plutusWitness script _data _redeemer defaultExunits)))  (Map.toList mp)

    toOuotput network (outCtx :: TxCtxOutput) =  do
      case outCtx of
        AddrCtxOut (addr,value) -> pure $ TxOut addr (TxOutValue MultiAssetInAlonzoEra value) TxOutDatumHashNone
        ScriptCtxOut (script,value,dataHash) -> pure $ TxOut (makeShelleyAddressInEra network (PaymentCredentialByScript  $  hashScript   (PlutusScript PlutusScriptV1   script)) NoStakeAddress) (TxOutValue MultiAssetInAlonzoEra  value ) (TxOutDatumHash ScriptDataInAlonzoEra dataHash)
        PkhCtxOut (pkh,value)-> case pkhToMaybeAddr network pkh of
          Nothing -> throw $ SomeError "PubKeyHash couldn't be converted to address"
          Just aie -> pure $ TxOut aie (TxOutValue MultiAssetInAlonzoEra value) TxOutDatumHashNone
    mkBody ins outs collateral pParam =
          (TxBodyContent {
            txIns=ins ,
            txInsCollateral=collateral,
            txOuts=outs,
            txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace 1000000,
            -- txValidityRange = (
            --     TxValidityLowerBound ValidityLowerBoundInAlonzoEra 0
            --     , TxValidityUpperBound ValidityUpperBoundInAlonzoEra 6969),
            txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
            txMetadata=TxMetadataNone ,
            txAuxScripts=TxAuxScriptsNone,
            txExtraScriptData=BuildTxWith TxExtraScriptDataNone ,
            txExtraKeyWits=TxExtraKeyWitnessesNone,
            txProtocolParams=BuildTxWith (Just  pParam),
            txWithdrawals=TxWithdrawalsNone,
            txCertificates=TxCertificatesNone,
            txUpdateProposal=TxUpdateProposalNone,
            txMintValue=TxMintNone,
            txScriptValidity=TxScriptValidityNone
          })
    plutusWitness script _data redeemer exUnits = PlutusScriptWitness PlutusScriptV1InAlonzo
                            PlutusScriptV1
                            script
                            (ScriptDatumForTxIn _data) -- script data
                            redeemer -- script redeemer
                            exUnits
    defaultExunits=ExecutionUnits {executionSteps=1794494645, executionMemory=5334093}
    isOnlyAdaTxOut (TxOut a v d) = case v of
                                        -- only ada then it's ok
                                        TxOutAdaOnly oasie lo -> True
                                        -- make sure that it has only one asset and that one is ada asset.
                                        TxOutValue masie va -> length vals == 1 && snd(head vals) > 0
                                              where
                                                vals=valueToList  va
    unWrapBalance f = do
      x <- f
      case  x  of
       Left tbe -> throw $ SomeError $ "First Balance :" ++ show tbe
       Right res -> pure res

mkBalancedBody :: ProtocolParameters
  -> UTxO AlonzoEra
  -> TxBodyContent BuildTx AlonzoEra
  -> Value
  -> AddressInEra AlonzoEra
  -> Word
  -> Either
      TxBodyError
      TxResult
mkBalancedBody  pParams (UTxO utxoMap)  txbody inputSum walletAddr signatureCount =
    do
      minLovelaceCalc <-case calculateTxoutMinLovelaceFunc pParams of
        Nothing -> Left TxBodyMissingProtocolParams
        Just f -> Right f

      -- first iteration
      let sanitizedOutputs = modifiedOuts minLovelaceCalc
          (inputs1,change1) =minimize txouts  $ startingChange txouts sanitizedOutputs startingFee
          txIns1=map utxoToTxBodyIn inputs1
          bodyContent1=modifiedBody sanitizedOutputs (map utxoToTxBodyIn inputs1) change1 startingFee
      txBody1 <- unEither $ case makeTransactionBody bodyContent1 of
        Left tbe -> Left $ SomeError $ show tbe
        Right tb -> Right  tb
      let modifiedChange1=change1 <> negLovelace  fee1 <> lovelaceToValue startingFee
          fee1= evaluateTransactionFee pParams txBody1 signatureCount 0
          (inputs2,change2)= minimizeConsideringChange minLovelaceCalc txouts (startingChange txouts sanitizedOutputs fee1)
          txIns2=map utxoToTxBodyIn inputs2
          bodyContent2 =modifiedBody sanitizedOutputs txIns2 change2 fee1
       -- if selected utxos are  sufficient to pay transaction fees, just use the fee and make txBody
       -- otherwide, reselect txins and recalculate fee. it's very improbable that the we will need more txouts now
      if positiveValue modifiedChange1 && isProperChange minLovelaceCalc modifiedChange1
        then do
          let  modifiedBody'=modifiedBody sanitizedOutputs txIns1 modifiedChange1 fee1
          txBody<-makeTransactionBody modifiedBody'
          Right (TxResult fee1 inputs1  modifiedBody'  txBody)
        else do
          txbody2 <- makeTransactionBody bodyContent2
          let fee2=evaluateTransactionFee pParams txbody2 1 0
              modifiedChange2= change2 <> negLovelace fee2 <> lovelaceToValue fee1
          if fee2 == fee1
            then Right  (TxResult fee2 inputs2 bodyContent2 txbody2)
            else do
              let body3=modifiedBody sanitizedOutputs txIns2 modifiedChange2 fee2
              txBody3 <- makeTransactionBody body3
              Right (TxResult fee2 inputs2 body3 txBody3)

  where
  performBalance sanitizedOuts  change fee= do
            let (inputs,change') =minimize txouts (change <> negLovelace fee)
                bodyContent=modifiedBody sanitizedOuts (map utxoToTxBodyIn inputs) change' fee
            txBody1<-makeTransactionBody bodyContent

            let modifiedChange1=change' <> negLovelace  fee' <> lovelaceToValue fee
                fee'= evaluateTransactionFee pParams txBody1 1 0
                (inputs2,change2)= minimize txouts modifiedChange1
                newBody =modifiedBody sanitizedOuts (map utxoToTxBodyIn inputs2) change2 fee'
            if fee' == fee
              then Right (bodyContent,change,fee)
              else Right (newBody, modifiedChange1,fee')

  startingFee=Lovelace $ toInteger $ protocolParamTxFeeFixed pParams

  negLovelace v=negateValue $ lovelaceToValue v

  txouts=  Map.toList utxoMap
  utxosWithWitness (txin,txout) = (txin, BuildTxWith  $ KeyWitness KeyWitnessForSpending)


  minimize utxos remainingChange= case utxos of
    []     -> ([] ,remainingChange)
    (txIn,txOut):subUtxos -> if val `valueLte` remainingChange
            then minimize subUtxos newChange -- remove the current txOut from the list
            else (case minimize subUtxos remainingChange of { (tos, va) -> ((txIn,txOut) :tos,va) }) -- include txOut in result
          where
            val = txOutValueToValue $ txOutValue  txOut
            newChange= remainingChange <> negateValue val

  minimizeConsideringChange f available change=if existingLove < minLove
    then
       (case Foldable.find (\(tin,utxo) -> extraLove utxo > (minLove- existingLove)) unmatched of
         Just val -> (fst matched ++ [val],snd matched <> txOutValueToValue ( txOutValue $ snd val ))
         Nothing -> matched
       )
    else
      matched
    where
      matched=minimize available change
      unmatched = Map.toList $ Map.filterWithKey  (\k _ -> k `notElem` matchedSet)  utxoMap
      matchedSet=Set.fromList $ map fst $  fst matched
      --Current Lovelace amount in the change utxo
      existingLove = case  selectAsset (snd  matched) AdaAssetId   of
        Quantity n -> n
      --minimun Lovelace required in the change utxo
      minLove = case  f $ TxOut walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumHashNone of
          Lovelace l -> l
      -- extra lovelace in this txout over the txoutMinLovelace
      extraLove txout = selectLove - minLoveInThisTxout
          where
            minLoveInThisTxout=case  f $ TxOut walletAddr (TxOutValue MultiAssetInAlonzoEra $ val <>change) TxOutDatumHashNone of
                Lovelace l -> l
            val= txOutValueToValue $ txOutValue txout
            selectLove = case selectAsset val AdaAssetId of { Quantity n -> n }

  isProperChange f change = existingLove >  minLove
    where
      existingLove = case  selectAsset change AdaAssetId   of
        Quantity n -> n
      --minimun Lovelace required in the change utxo
      minLove = case  f $ TxOut walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumHashNone of
          Lovelace l -> l


  utxoToTxBodyIn (txIn,_) =(txIn,BuildTxWith $ KeyWitness KeyWitnessForSpending)

  -- minimize' utxos remainingChange = (doMap,remainingChange)
  --   where
  --     doMap=map (\(txin,txout) -> tobodyIn txin) utxos
  --     tobodyIn _in=(_in,BuildTxWith $ KeyWitness KeyWitnessForSpending)
  --     val  out= txOutValueToValue $ txOutValue  out



  -- change is whatever will remain after making payment.
  -- At the beginning, we will assume that we will all the available utxos, 
  -- so it should be a +ve value, otherwise it means we don't have sufficient balance to fulfill the transaction 
  startingChange available outputs  fee=
        negateValue (foldMap (txOutValueToValue  . txOutValue ) outputs)  --already existing outputs
    <>  (if  null (txIns txbody) then mempty else inputSum) -- already existing inputs
    <>  Foldable.foldMap (txOutValueToValue  . txOutValue . snd) available -- sum of all the available utxos
    <>  negateValue (lovelaceToValue fee)
  utxoToTxOut (UTxO map)=Map.toList map

  txOutValueToValue :: TxOutValue era -> Value
  txOutValueToValue tv =
    case tv of
      TxOutAdaOnly _ l -> lovelaceToValue l
      TxOutValue _ v -> v

  txOutValue (TxOut _ v _) = v

  modifiedOuts calculator = map (includeMin calculator) (txOuts  txbody)
  includeMin calculator txOut= do case txOut of {TxOut addr v hash-> case v of
                                     TxOutAdaOnly oasie lo ->  txOut
                                     TxOutValue masie va ->
                                       if selectAsset va AdaAssetId == Quantity  0
                                       then performMinCalculation addr va hash
                                       else  txOut }
    where
      performMinCalculation addr val hash =TxOut  addr (TxOutValue MultiAssetInAlonzoEra  (val <> lovelaceToValue minLovelace)) hash
        where
         minLovelace = minval addr (val <> lovelaceToValue (Lovelace 1_000_000)) hash

      minval add v hash= calculator (TxOut add (TxOutValue MultiAssetInAlonzoEra v) hash )

  modifiedBody initialOuts txins change fee= content
    where
      reorderInputs :: [(TxIn, a)] -> [(TxIn,a)]
      reorderInputs txIns = map ((\key-> (key,forceJust $ Map.lookup key  mapped)) . fromShelleyTxIn) ordered
        where
          ordered=   Set.toList $  Set.fromList (map (toShelleyTxIn . fst) txIns)
          mapped= Map.fromList txIns
          forceJust (Just a) = a

      content=(TxBodyContent  {
            txIns= reorderInputs$ txins ++ txIns txbody,
            txInsCollateral=txInsCollateral txbody,
            txOuts=  if nullValue change
                  then initialOuts
                  else initialOuts ++ [ TxOut  walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumHashNone]  ,
            txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra  fee,
            -- txValidityRange=(TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
            txValidityRange = txValidityRange txbody,
            txMetadata=txMetadata txbody ,
            txAuxScripts=txAuxScripts txbody,
            txExtraScriptData=txExtraScriptData txbody ,
            txExtraKeyWits=txExtraKeyWits txbody,
            txProtocolParams= txProtocolParams   txbody,
            txWithdrawals=txWithdrawals txbody,
            txCertificates=txCertificates txbody,
            txUpdateProposal=txUpdateProposal txbody,
            txMintValue=txMintValue txbody,
            txScriptValidity=txScriptValidity txbody
          })



executeMkBalancedBody :: Applicative f =>
  ProtocolParameters
  -> UTxO AlonzoEra
  -> TxBodyContent BuildTx AlonzoEra
  -> Value
  -> AddressInEra AlonzoEra
  -> Word
  -> f TxResult
executeMkBalancedBody  pParams utxos  txbody inputSum walletAddr signatureCount=do
  let balancedBody=mkBalancedBody pParams utxos txbody inputSum walletAddr signatureCount
  case balancedBody of
    Left e -> throw $ SomeError $ show e
    Right v ->pure v


valueLte :: Value -> Value -> Bool
valueLte _v1 _v2= not $ any (\(aid,Quantity q) -> q > lookup aid) (valueToList _v1) -- do we find anything that's greater than q
  where
    lookup x= case Map.lookup x v2Map of
      Nothing -> 0
      Just (Quantity v) -> v
    v2Map=Map.fromList $ valueToList _v2
    -- v1Bundle= case case valueToNestedRep _v1 of { ValueNestedRep bundle -> bundle} of
    --   [ValueNestedBundleAda v , ValueNestedBundle policy assetMap] ->LovelaceToValue v
    --   [ValueNestedBundle policy assetMap]


balanceAndSubmitBody conn sKey body utxos sum  = do
  pParam <-case txProtocolParams body of {
    BuildTxWith m_pp -> case m_pp of
        Just pp -> pure pp
        Nothing -> queryProtocolParam conn

    }
  let balancedBody = mkBalancedBody pParam  utxos body sum    (skeyToAddrInEra sKey (localNodeNetworkId conn))  1

  submitEitherBalancedBody conn  balancedBody [sKey]

submitEitherBalancedBody ::
  LocalNodeConnectInfo CardanoMode
  ->  Either TxBodyError TxResult
  -> [SigningKey PaymentKey]
  -> IO (Tx AlonzoEra)
submitEitherBalancedBody conn eitherBalancedBody skeys =
      --End Balance transaction body with fee
  case  eitherBalancedBody of
    Left tbe ->
      throw $ SomeError $  "Coding Error : Balanced TxBody has error : " ++  show tbe
    Right (TxResult _ _ _ txBody) ->signAndSubmitTxBody conn txBody skeys

