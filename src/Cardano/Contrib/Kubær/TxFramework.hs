{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, maybeToList)
import Data.List (intercalate, sortBy)
import qualified Data.Foldable as Foldable
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), Validator (Validator), unValidatorScript )
import Cardano.Contrib.Kubær.TxBuilder
import Cardano.Contrib.Kubær.ChainInfo (DetailedChainInfo (DetailedChainInfo, dciConn), ChainInfo (getNetworkId))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Cardano.Api.Crypto.Ed25519Bip32 (xPrvFromBytes)
import Debug.Trace (trace, traceM)
import qualified Data.Aeson as A

type IsChangeUsed   = Bool
type  ParsedInput   = Either (Witness WitCtxTxIn AlonzoEra,TxOut CtxUTxO AlonzoEra) (Maybe ExecutionUnits,ScriptWitness WitCtxTxIn AlonzoEra,TxOut CtxUTxO  AlonzoEra)
type  ParsedOutput  = (Bool,Bool,TxOut CtxTx AlonzoEra)




txBuilderToTxBody:: DetailedChainInfo  -> TxBuilder  -> IO (Either FrameworkError  (TxBody AlonzoEra ))
txBuilderToTxBody dcInfo builder = do
  let (selectionAddrs,txins,utxo) = mergeSelections
      addrs=  Set.union selectionAddrs (Set.fromList $ mapMaybe getInputAddresses (txInputs builder))
  addrUtxos <- queryIfNotEmpty addrs (queryUtxos  conn addrs) (Right $ UTxO  Map.empty)
  case addrUtxos of
    Left fe -> pure $ Left fe
    Right (UTxO  uto) -> do
      let missingTxins= Set.difference txins ( Map.keysSet  uto )
      vals <- queryIfNotEmpty missingTxins (resolveTxins conn missingTxins) (Right $ UTxO  Map.empty)
      case vals of
        Left fe -> pure $ Left fe
        Right (UTxO uto') ->do
          pure $ mkTx dcInfo (UTxO $ Map.union uto uto') builder
  where

    queryIfNotEmpty v f v' = if null  v then pure v' else f
    conn=dciConn dcInfo
    mergeSelections=foldl mergeSelection (Set.empty,Set.empty ,Map.empty ) (txSelections builder)
    getInputAddresses :: TxInput -> Maybe AddressAny
    getInputAddresses x = case x of
      TxInputResolved tir -> Nothing
      TxInputUnResolved (TxInputAddr aie) -> Just $ addressInEraToAddressAny aie
      _ -> Nothing
    mergeSelection :: ( Set AddressAny,Set TxIn, Map TxIn (TxOut CtxUTxO AlonzoEra))  -> TxInputSelection  -> (Set AddressAny,Set TxIn, Map TxIn (TxOut CtxUTxO AlonzoEra))
    mergeSelection (a,i,u) sel = case sel of
        TxSelectableAddresses aies -> (Set.union a  (Set.fromList $ map addressInEraToAddressAny aies),i,u)
        TxSelectableUtxos (UTxO uto) -> (a,i,merge uto u)
        TxSelectableTxIn tis -> (a,Set.union i (Set.fromList tis),u)

merge :: Map TxIn (TxOut CtxUTxO AlonzoEra) -> Map TxIn (TxOut CtxUTxO AlonzoEra) -> Map TxIn (TxOut CtxUTxO AlonzoEra)
merge = error "not implemented"

mkTx::DetailedChainInfo ->  UTxO AlonzoEra -> TxBuilder   -> Either FrameworkError  (TxBody AlonzoEra)
mkTx  dCinfo@(DetailedChainInfo cpw conn pParam systemStart eraHisotry ) (UTxO availableUtxo) (TxBuilder selections _inputs _outputs mintingScripts collaterals validityStart validityEnd mintValue extraSignatures explicitFee mChangeAddr ) = do
  let network = getNetworkId  dCinfo
  fixedInputs <- mapM resolveInputs _inputs >>= usedInputs Map.empty
  fixedOutputs <- mapM (parseOutputs network) _outputs

  let fixedInputSum =  usedInputSum fixedInputs <> mintValue
      fee= Lovelace 100_000
      availableInputs = sortUtxos $ UTxO  $ Map.filterWithKey (\ tin _ -> Map.notMember tin fixedInputs) availableUtxo
      calculator= computeBody fixedInputs fixedInputSum availableInputs fixedOutputs
  (newBody,fee2) <-  calculator  fee
  (newbody,fee3) <- calculator fee2
  if fee2 /= fee3  then Left $ FrameworkError LibraryError "Transaction not balanced even in 3rd iteration" else pure  ()
  pure newbody
  where
    computeBody ::  Map TxIn ParsedInput
      -> Value
      -> [(TxIn, TxOut CtxUTxO AlonzoEra)]
      -> [ParsedOutput]
      -> Lovelace
      -> Either FrameworkError  (TxBody AlonzoEra, Lovelace)
    computeBody fixedInputs fixedInputSum availableInputs fixedOutputs fee  = do
      let
        startingChange=   fixedInputSum <>   negateValue(fixedOutputSum<> if _hasFeeUtxo then mempty else lovelaceToValue fee )
        (extraUtxos,change) = selectUtxos availableInputs startingChange
        _hasFeeUtxo = any (\(a,b,c)->a) fixedOutputs
        (feeUsed,changeUsed,outputs) = updateOutputs  fee change fixedOutputs
        bodyContent allOutputs = mkBodyContent  fixedInputs extraUtxos allOutputs [] fee
      bc <- if changeUsed 
              then pure $ bodyContent outputs
              else do
                changeaddr <-  monadFailChangeAddr
                pure $ bodyContent (outputs++ [TxOut changeaddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumNone])
      case makeTransactionBody bc of
          Left tbe ->Left  $ FrameworkError  LibraryError  (show tbe)
          Right tb -> pure (tb,evaluateTransactionFee pParam tb 1 0)

      where
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
          TxSelectableUtxos uto -> Nothing
          TxSelectableTxIn tis -> Nothing
    getTxin :: Map TxIn ParsedInput -> [(TxIn,TxOut CtxUTxO AlonzoEra )]-> [(TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra ))]
    getTxin v  v2 = map ( uncurry totxIn)  (Map.toList v) ++ map toPubKeyTxin v2

    toPubKeyTxin :: (TxIn,a) -> (TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra ))
    toPubKeyTxin (v1,v2) =(v1,BuildTxWith $ KeyWitness KeyWitnessForSpending )

    totxIn :: TxIn ->  ParsedInput -> (TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra ))
    totxIn  i  parsedInput = case parsedInput of
      Left (a,b) -> (i,BuildTxWith a)
      Right (e,a,b) -> (i,BuildTxWith  ( ScriptWitness ScriptWitnessForSpending a )  )
    mkBodyContent fixedInputs extraUtxos outs collateral  fee =
      (TxBodyContent {
        txIns= getTxin fixedInputs extraUtxos ,
        txInsCollateral= if null collateral then TxInsCollateralNone  else TxInsCollateral CollateralInAlonzoEra collateral,
        txOuts=outs,
        Cardano.Api.Shelley.txFee=TxFeeExplicit TxFeesExplicitInAlonzoEra  fee,
        txValidityRange= (txLowerBound,txUpperBound),
        txMetadata=TxMetadataNone ,
        txAuxScripts=TxAuxScriptsNone,
        txExtraKeyWits=TxExtraKeyWitnessesNone,
        txProtocolParams=BuildTxWith (Just  pParam),
        txWithdrawals=TxWithdrawalsNone,
        txCertificates=TxCertificatesNone,
        txUpdateProposal=TxUpdateProposalNone,
        txMintValue=TxMintNone,
        txScriptValidity=TxScriptValidityNone
          })
    fixedOutputSum = foldMap txOutputVal _outputs
      where
      txOutputVal :: TxOutput -> Value
      txOutputVal o = case o of { TxOutput toc b b' -> case toc of
                                    TxOutAddress aie va -> va
                                    TxOutScriptAddress aie va ha -> va
                                    TxOutPkh pkh va -> va
                                    TxOutScript tvs va ha -> va  }
    zeroValue = valueFromList []
    updateOutputs  fee change outputs' = updateOutput False False (getNetworkId  dCinfo) fee change outputs'
    updateOutput :: Bool -> Bool -> NetworkId -> Lovelace -> Value -> [ParsedOutput] ->  (Bool,Bool,[TxOut CtxTx AlonzoEra])
    updateOutput _ _ _ _ _ []  =  (False,False,[])
    updateOutput _fUsed _cUsed network (Lovelace fee) change (txOutput:outs) =let
        (feeUsed,changeUsed,result) = transformOut _fUsed _cUsed txOutput
        (feeUsed2,changeUsed2,others) = updateOutput feeUsed changeUsed network (Lovelace fee) change outs
        in   (feeUsed  || feeUsed2 , changeUsed || changeUsed2, result : others )
      where
        transformOut changeUsed feeUsed (addFee,addChange,TxOut aie (TxOutValue _ va) ha)=  (feeUsed'',changeUsed'',TxOut aie (TxOutValue MultiAssetInAlonzoEra modifiedVal) ha)
          where
            (feeUsed'',changeUsed'',modifiedVal) = includeFeeNChange va
            includeFeeNChange va = let  (feeUsed',feeIncluded) = includeFee va
                                        (changeUsed', changeIncluded) = includeChange feeIncluded
                                    in (feeUsed',changeUsed',changeIncluded)
            includeFee val
              | feeUsed = (True, val)
              | addFee = (True, valueFromList [(AdaAssetId ,Quantity (- fee))] <> val)
              | otherwise = (False,val)

            includeChange val
              | changeUsed = (True,val)
              | addChange = (True,change<> val)
              | otherwise = (False,val)
        transformOut _ _ _ = error "UnExpected condition"


    parseOutputs :: MonadFail m => NetworkId -> TxOutput -> m ParsedOutput
    parseOutputs  networkId output = case output of { TxOutput toc b b' -> case toc of
    parseOutputs ::  TxOutput -> Either FrameworkError   ParsedOutput
    parseOutputs  output = case output of { TxOutput toc b b' -> case toc of
                                              TxOutAddress aie va -> pure  (b,b',TxOut aie  (TxOutValue MultiAssetInAlonzoEra va ) TxOutDatumNone )
                                              TxOutScriptAddress aie va ha -> pure (b,b',TxOut aie (TxOutValue MultiAssetInAlonzoEra va) (TxOutDatumHash ScriptDataInAlonzoEra ha ))
                                              TxOutPkh pkh va -> case pkhToMaybeAddr (getNetworkId  dCinfo) pkh of
                                                      Nothing -> Left  $ FrameworkError ParserError  ("Cannot convert PubKeyHash to Address : "++ show pkh)
                                                      Just aie ->  pure  (b,b',TxOut aie  (TxOutValue MultiAssetInAlonzoEra va ) TxOutDatumNone )
                                              TxOutScript tvs va ha -> Left  $ FrameworkError ParserError "Not Implemented : Calculation of script address from the script binary is not supported"  }


    resolveInputs ::  TxInput -> Either FrameworkError    TxInputResolved_
                                              TxOutScript (TxValidatorScript (ScriptInAnyLang lang script)) va ha ->
                                                let payCred = PaymentCredentialByScript (hashScript script)
                                                    addr = makeShelleyAddress networkId payCred NoStakeAddress
                                                    addrInEra = AddressInEra (ShelleyAddressInEra ShelleyBasedEraAlonzo) addr
                                                in pure (b,b',TxOut addrInEra (TxOutValue MultiAssetInAlonzoEra va) (TxOutDatumHash ScriptDataInAlonzoEra ha ))}

    resolveInputs :: MonadFail m => TxInput -> m  TxInputResolved_
    resolveInputs v = case v of
      TxInputResolved tir -> pure tir
      TxInputUnResolved (TxInputTxin txin) ->  doLookup txin <&> TxInputUtxo
      TxInputUnResolved (TxInputAddr addr) ->   filterAddrUtxo addr <&> TxInputUtxo
      TxInputUnResolved (TxInputScriptTxin s d r exunit txin) -> doLookup txin <&>  TxInputScriptUtxo s d r exunit
      where
        filterAddrUtxo addr =pure $ UTxO $ Map.filter (ofAddress addr) availableUtxo
        ofAddress addr (TxOut a _ _)= addr == a
        doLookup tin = case Map.lookup tin availableUtxo of
          Nothing -> Left $ FrameworkError LibraryError  "Input Utxo missing in utxo map"
          Just to ->pure $ UTxO $ Map.singleton  tin  to
    mapInputs  exlookup is=do
          tuples<- mapM (toInput exlookup) is
          pure $ Map.fromList $ concat tuples
    toInput ::  Map TxIn ExecutionUnits -> TxInputResolved_-> Either FrameworkError   [(TxIn,ParsedInput)]
    toInput exUnitLookup inCtx = case inCtx of
      TxInputUtxo (UTxO txin) ->  pure $ map (\(_in,val) -> (_in,Left (  KeyWitness KeyWitnessForSpending, val) ))  $ Map.toList txin
      TxInputScriptUtxo (TxValidatorScript s) d r mExunit (UTxO txin) ->mapM (\(_in,val) -> do
                                                                witness <-  createTxInScriptWitness s d r (getExUnit _in mExunit)
                                                                pure (_in,Right (mExunit, witness,val )) ) $ Map.toList txin
      where

        getExUnit tin ex =case  ex of
          Just ex -> ex
          Nothing -> fromMaybe defaultExunits (Map.lookup tin exUnitLookup)

    usedInputs ::  Map TxIn ExecutionUnits -> [TxInputResolved_] -> Either FrameworkError  (Map TxIn ParsedInput)
    usedInputs exUnitLookup resolvedInputs = do
      vs<- mapM (toInput exUnitLookup) resolvedInputs
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
