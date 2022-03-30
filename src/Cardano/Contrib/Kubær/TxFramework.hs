{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Contrib.Kubær.TxFramework where


import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Contrib.Kubær.Error
import PlutusTx (ToData)
import Cardano.Slotting.Time
import qualified Cardano.Ledger.Alonzo.TxBody as LedgerBody
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Control.Exception
import Data.Either
import Cardano.Contrib.Kubær.Util
import Data.Functor ((<&>))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)
import Data.Set (Set)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript)
import Cardano.Contrib.Kubær.TxBuilder
import Cardano.Contrib.Kubær.ChainInfo (DetailedChainInfo (DetailedChainInfo), ChainInfo (getNetworkId))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)


mkTx:: DetailedChainInfo ->  UTxO AlonzoEra -> TxBuilder   -> Either String (TxBody AlonzoEra)
mkTx  dCinfo@(DetailedChainInfo cpw conn pParam systemStart eraHisotry ) (UTxO availableUtxo) (TxBuilder selections inputs outputs mintingScripts collaterals validityStart validityEnd mintValue extraSignatures explicitFee defaultChangeAddr ) = do
  let network = getNetworkId  dCinfo
  -- walletAddr <-unMaybe (SomeError "unexpected error converting address to another type") (deserialiseAddress AsAddressAny (serialiseAddress  payerAddrInEra))
  -- UTxO _allWalletUtxos <- queryUtxos conn walletAddr
  -- mappedOutput <- mapM (toOuotput (networkCtxNetwork networkCtx)) output
  -- (operationUtxos,_txins) <- resolveAndSumIns conn input
  -- let operationUtoSum=foldMap txOutValue (Map.elems operationUtxos)
  -- let unUnsedWalletUtos = Map.filterWithKey (\k  a -> k `Map.notMember` operationUtxos) _allWalletUtxos

  error " fail"

    -- let eExunits= evaluateTransactionExecutionUnits AlonzoEraInCardanoMode systemStart eraHistory pParam usedUtxos balancedRevision0
    -- case eExunits of
    --   Left tvie ->do 
    --       putStrLn  "[WARNING] :: exUnit calculation failed. Using default"  -- throw $ SomeError $ "ExecutionUnitCalculation :" ++ show tvie
    --       pure $ TxResult fee usedWalletUtxos balancedRevisionContent0 balancedRevision0
    --   Right mp ->do 
    --     case applyTxInExecutionUnits _txins (txIns balancedRevisionContent0) orderedIns mp of
    --       Left se -> do 
    --         putStrLn $ "[WARNING] :: exUnit application failed. Using default :" ++ show se  -- throw $ SomeError $ "ExecutionUnitCalculation :" ++ show tvie
    --         pure $ TxResult fee usedWalletUtxos balancedRevisionContent0 balancedRevision0
    --       Right modifiedIns -> do 
    --         executeMkBalancedBody pParam (UTxO walletUtxos)  (mkBody  modifiedIns mappedOutput txInsCollateral pParam)  operationUtoSum changeAddrInEra signatureCount)

  where
    resolveInputs :: MonadFail m => TxInput -> m  TxInputResolved_
    resolveInputs v = case v of
      TxInputResolved tir -> pure tir
      TxInputUnResolved (TxInputTxin txin) ->  doLookup txin <&> TxInputUtxo
      TxInputUnResolved (TxInputScriptTxin s d r exunit txin) -> doLookup txin <&>  TxInputScriptUtxo s d r exunit
      where
        doLookup tin = case Map.lookup tin availableUtxo of
          Nothing -> fail "Missing utxo"
          Just to ->pure $ UTxO $ Map.singleton  tin  to
    mapInputs  exlookup is=do   
          tuples<- mapM (toInput exlookup) is
          pure $ Map.fromList $ concat tuples
    toInput :: MonadFail m => Map TxIn ExecutionUnits -> TxInputResolved_-> m [(TxIn,Either 
                                                                        (BuildTxWith BuildTx (Witness WitCtxTxIn era))
                                                                        (Maybe ExecutionUnits,BuildTxWith BuildTx (ScriptWitness WitCtxTxIn AlonzoEra)))]
    toInput exUnitLookup ( inCtx ::TxInputResolved_)= case inCtx of
      TxInputUtxo (UTxO txin) ->  pure $ map (\(_in,val) -> (_in,Left $ BuildTxWith $ KeyWitness KeyWitnessForSpending) )  $ Map.toList txin
      TxInputScriptUtxo (TxValidatorScript s) d r mExunit (UTxO txin) ->mapM (\(_in,val) -> do 
                                                                witness <-  createTxInScriptWitness s d r (getExUnit _in mExunit)
                                                                pure (_in,Right (mExunit,BuildTxWith witness )) ) $ Map.toList txin
      where

        getExUnit tin ex =case  ex of
          Just ex -> ex
          Nothing -> fromMaybe defaultExunits (Map.lookup tin exUnitLookup)

    --  UtxoCtxIn (UTxO mp) -> Right $  map (\item-> (item,(fst item,BuildTxWith $ KeyWitness KeyWitnessForSpending)))   (Map.toList mp)
    --   TxInCtxIn ti -> Left (ti,BuildTxWith $ KeyWitness KeyWitnessForSpending)
    --   ScriptCtxTxIn (script,_data,_redeemer,txin) -> Left (txin,BuildTxWith $  ScriptWitness ScriptWitnessForSpending $ plutusWitness script _data _redeemer defaultExunits)
    --   ScriptUtxoCtxTxIn (script, _data,_redeemer,UTxO mp) -> Right $ map (\item-> (item,(fst  item,BuildTxWith $  ScriptWitness ScriptWitnessForSpending $ plutusWitness script _data _redeemer defaultExunits)))  (Map.toList mp)

    -- signatureCount = fromIntegral $ length signature + 1
    -- hasScriptInput = any (\case
    --                           ScriptCtxTxIn x0 -> True
    --                           ScriptUtxoCtxTxIn x0 -> True
    --                           _ -> False )  input

    -- unEitherExecutionUnit e= case e of
    --   Left e -> throw $  SomeError  $ "EvaluateExecutionUnits: " ++ show e
    --   Right v -> pure v
    -- resolveAndSumIns conn inputs = if null unresolved
    --   then pure (Map.fromList $ map  fst  resolved,map snd resolved)
    --   else do
    --     (UTxO utos) <-queryTxins conn (map fst unresolved )
    --     if length utos /= length unresolved then throw (SomeError "QueryUtxoValue: Requested utxos not found") else pure ()
    --     let fixedTxins=map snd resolved ++ unresolved
    --     let usedUtxos=  Map.union  utos  (Map.fromList $map fst resolved)
    --     pure (usedUtxos,fixedTxins)
    --     where
    --       all= partitionEithers vs
    --       resolved=concat $ snd all
    --       unresolved=fst all
    --       vs=map toInput inputs
    -- mappedOpCollateral  =   map (\(TxInCollateral _in) -> _in) oPcollaterals
    -- txOutValue (TxOut _ v _) = case v of
    --   TxOutAdaOnly oasie lo -> lovelaceToValue lo
    --   TxOutValue masie va -> va


    -- toOuotput network (outCtx :: TxCtxOutput) =  do
    --   case outCtx of
    --     AddrCtxOut (addr,value) -> pure $ TxOut addr (TxOutValue MultiAssetInAlonzoEra value) TxOutDatumNone
    --     ScriptCtxOut (script,value,dataHash) -> pure $ TxOut (makeShelleyAddressInEra network (PaymentCredentialByScript  $  hashScript   (PlutusScript PlutusScriptV2   script)) NoStakeAddress) (TxOutValue MultiAssetInAlonzoEra  value ) (TxOutDatumHash ScriptDataInAlonzoEra dataHash)
    --     PkhCtxOut (pkh,value)-> case pkhToMaybeAddr network pkh of
    --       Nothing -> throw $ SomeError "PubKeyHash couldn't be converted to address"
    --       Just aie -> pure $ TxOut aie (TxOutValue MultiAssetInAlonzoEra value) TxOutDatumNone
    -- mkBody ins outs collateral pParam =
    --       (TxBodyContent {
    --         txIns=ins ,
    --         txInsCollateral=collateral,
    --         txOuts=outs,
    --         txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace 1000000,
    --         -- txValidityRange = (
    --         --     TxValidityLowerBound ValidityLowerBoundInAlonzoEra 0
    --         --     , TxValidityUpperBound ValidityUpperBoundInAlonzoEra 6969),
    --         txValidityRange= if validityStart == 0
    --                           then  if  validityEnd == 0
    --                                   then  (TxValidityNoLowerBound,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra )
    --                                   else (TxValidityNoLowerBound , TxValidityUpperBound  ValidityUpperBoundInAlonzoEra $toSlot validityEnd)
    --                           else  if validityEnd == 0
    --                                   then (TxValidityLowerBound ValidityLowerBoundInAlonzoEra  $ toSlot validityStart,TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra )
    --                                   else (TxValidityLowerBound  ValidityLowerBoundInAlonzoEra $ toSlot validityStart,TxValidityUpperBound ValidityUpperBoundInAlonzoEra $ toSlot validityEnd)
    --           ,
    --         txMetadata=TxMetadataNone ,
    --         txAuxScripts=TxAuxScriptsNone,
    --         txExtraKeyWits=TxExtraKeyWitnessesNone,
    --         txProtocolParams=BuildTxWith (Just  pParam),
    --         txWithdrawals=TxWithdrawalsNone,
    --         txCertificates=TxCertificatesNone,
    --         txUpdateProposal=TxUpdateProposalNone,
    --         txMintValue=TxMintNone,
    --         txScriptValidity=TxScriptValidityNone
    --       })
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

    -- toSlot tStamp= case networkCtxNetwork networkCtx of
    --   Mainnet -> SlotNo $ fromIntegral $  mainnetSlot tStamp
    --   Testnet nm -> SlotNo $ fromIntegral $ testnetSlot tStamp
    -- testnetSlot timestamp= ((timestamp -1607199617000) `div` 1000 )+ 12830401 -- using epoch 100 as refrence
    -- mainnetSlot timestamp = ((timestamp -1596491091000 ) `div` 1000 )+ 4924800 -- using epoch 209 as reference

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

--   txouts :: [(TxIn,TxOut CtxUTxO AlonzoEra )]
--   txouts = sortBy sortingFunc ( Map.toList utxoMap)

--   -- sort the txouts based on following condition
--   -- - the ones with multiple assets comes first
--   -- - then the ones with lower lovelace amount come
--   -- - then the ones with higher lovelace amount come
--   sortingFunc :: (TxIn,TxOut CtxUTxO AlonzoEra) -> (TxIn,TxOut CtxUTxO AlonzoEra)-> Ordering
--   sortingFunc (_,TxOut _ (TxOutAdaOnly _ v1) _) (_, TxOut _ (TxOutAdaOnly _ v2)  _) = v1 `compare` v2
--   sortingFunc (_,TxOut _ (TxOutAdaOnly _ (Lovelace v))  _) (_, TxOut _ (TxOutValue _ v2) _) = LT 
--   sortingFunc (_,TxOut _ (TxOutValue _ v1) _) (_, TxOut _ (TxOutAdaOnly _ v2) _) =  GT
--   sortingFunc (_,TxOut _ (TxOutValue _ v1) _) (_, TxOut _ (TxOutValue _ v2) _) =  let l1= length ( valueToList v1)
--                                                                                       l2= length (valueToList v2) in
--                                                                                   if l1==l2
--                                                                                     then selectAsset v1 AdaAssetId `compare` selectAsset v2  AdaAssetId
--                                                                                     else l2 `compare` l1


--   utxosWithWitness (txin,txout) = (txin, BuildTxWith  $ KeyWitness KeyWitnessForSpending)


--   -- from the utxos, try to remove utxos that can be removed while keeping the change positive or zero if possible
--   minimize utxos remainingChange= case utxos of
--     []     -> ([] ,remainingChange)
--     (txIn,txOut):subUtxos -> if val `valueLte` remainingChange
--             then minimize subUtxos newChange -- remove the current txOut from the list
--             else (case minimize subUtxos remainingChange of { (tos, va) -> ((txIn,txOut) :tos,va) }) -- include txOut in result
--           where
--             val = txOutValueToValue $ txOutValue  txOut
--             newChange= remainingChange <> negateValue val

--    -- consider change while minimizing i.e. make sure that the change has the minLovelace value.
--   minimizeConsideringChange f available change=if existingLove < minLove
--     then
--        (case Foldable.find (\(tin,utxo) -> extraLove utxo > (minLove- existingLove)) unmatched of
--          Just val -> (fst matched ++ [val],snd matched <> txOutValueToValue ( txOutValue $ snd val ))
--          Nothing -> matched
--        )
--     else
--       matched
--     where
--       matched=minimize available change
--       unmatched = Map.toList $ Map.filterWithKey  (\k _ -> k `notElem` matchedSet)  utxoMap
--       matchedSet=Set.fromList $ map fst $  fst matched
--       --Current Lovelace amount in the change utxo
--       existingLove = case  selectAsset (snd  matched) AdaAssetId   of
--         Quantity n -> n
--       --minimun Lovelace required in the change utxo
--       minLove = case  f $ TxOut walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumNone of
--           Lovelace l -> l
--       -- extra lovelace in this txout over the txoutMinLovelace
--       extraLove txout = selectLove - minLoveInThisTxout
--           where
--             minLoveInThisTxout=case  f $ TxOut walletAddr (TxOutValue MultiAssetInAlonzoEra $ val <>change) TxOutDatumNone of
--                 Lovelace l -> l
--             val= txOutValueToValue $ txOutValue txout
--             selectLove = case selectAsset val AdaAssetId of { Quantity n -> n }

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
--       reorderInputs :: [(TxIn, a)] -> [(TxIn,a)]
--       reorderInputs txIns = map ((\key-> (key,forceJust $ Map.lookup key  mapped)) . fromShelleyTxIn) ordered
--         where
--           ordered=   Set.toList $  Set.fromList (map (toShelleyTxIn . fst) txIns)
--           mapped= Map.fromList txIns
--           forceJust (Just a) = a

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




-- mkTxExplicitFee ::DetailedChainInfo -> TxBuilder -> TxBody AlonzoEra
-- mkTxExplicitFee = error "sad"

-- gatherInfo :: ChainInfo i -> i  -> TxBuilder  ->  IO (Either AcquireFailure TxContext)
-- gatherInfo cInfo  txBuilder@TxBuilder{txSelections, txInputs} = do 
--   error "sad"
--   where
