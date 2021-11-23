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

data TestCtxIn  = UtxoCtxIn (UTxO AlonzoEra) | TxInCtxIn TxIn | ScriptCtxTxIn(PlutusScript PlutusScriptV1,ScriptData,ScriptData,TxIn) | ScriptUtxoCtxTxIn(PlutusScript PlutusScriptV1,ScriptData,ScriptData,UTxO AlonzoEra)deriving (Show)
data TestCtxOut =
    AddrCtxOut (AddressInEra AlonzoEra,Value)
  | PkhCtxOut (PubKeyHash,Value)
  | ScriptCtxOut (PlutusScript PlutusScriptV1,Value, Hash ScriptData)  deriving (Show)


-- Context Builder for test transaction
-- You will not use this directly, instead use builder functions
-- to compose this structure.
data TxOperationBuilder=TxOperationBuilder{
    ctxInputs:: [TestCtxIn], -- inputs in this transaction
    ctxOutputs::[TestCtxOut], -- outputs in this transaction
    ctxSignatures :: [SigningKey PaymentKey] -- public key signatures in this transaction
  } deriving(Show)



instance Semigroup TxOperationBuilder where
  (<>) ctx1 ctx2=TxOperationBuilder{
    ctxInputs=ctxInputs ctx1 ++ ctxInputs ctx2,
    ctxOutputs=ctxOutputs ctx1 ++ ctxOutputs ctx2,
    ctxSignatures=ctxSignatures ctx1 ++ctxSignatures ctx2
  }

instance Monoid TxOperationBuilder where
  mempty = TxOperationBuilder [] [] [] 
-- mkTxWithWallet :: SigningKey PaymentKey -> TxOperationBuilder -> Ledger.Tx

-- In this transaction, pay some value to the wallet.
-- It will be included in the tx_out of this transaction
txPayTo:: AddressInEra AlonzoEra ->Value ->TxOperationBuilder
txPayTo addr v= TxOperationBuilder{
    ctxInputs=[],
    ctxOutputs=[AddrCtxOut  (addr, v)],
    ctxSignatures=[] 
  }

txConsumeUtxos :: UTxO AlonzoEra -> TxOperationBuilder
txConsumeUtxos utxo = TxOperationBuilder{
  ctxInputs=[UtxoCtxIn  utxo],
  ctxOutputs=[],
  ctxSignatures=[]
}


txPayToPkh:: PubKeyHash  ->Value ->TxOperationBuilder
txPayToPkh pkh v= TxOperationBuilder{
    ctxInputs=[],
    ctxOutputs=[PkhCtxOut  (pkh, v)],
    ctxSignatures=[]
  }

-- In this transaction send  x lovelace to an wallet.
-- It will appear in tx_out
txPayLovelaceTo :: AddressInEra AlonzoEra  -> Integer -> TxOperationBuilder
txPayLovelaceTo w v=txPayTo w (lovelaceToValue $quantityToLovelace $ Quantity v)


-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.
txPayToScript:: ToData _data=>PlutusScript PlutusScriptV1 -> _data -> Value->TxOperationBuilder
txPayToScript script _data v =TxOperationBuilder{
    ctxInputs=[],
    ctxOutputs=[ScriptCtxOut (script,v,hashScriptData  $ dataToScriptData   _data)],
    ctxSignatures=[]
  }

-- Lock value and data in a script.
-- It's a script that we depend on. but we are not testing it.
-- So, the validator of this script will not be executed.
txPayToValidator:: ToData _data=>Validator  -> _data -> Value->TxOperationBuilder
txPayToValidator validator _data v =TxOperationBuilder{
    ctxInputs=[],
    ctxOutputs=[ScriptCtxOut (PlutusScriptSerialised serialisedScript ,v,hashScriptData  $ dataToScriptData   _data)],
    ctxSignatures=[]
  }
  where
    serialisedScript :: SBS.ShortByteString
    serialisedScript = SBS.toShort . LBS.toStrict $ serialise script
    script  = Plutus.unValidatorScript validator



-- Redeem from Script Address.
txRedeem:: (ToData _data,ToData redeemer)=>TxIn -> PlutusScript PlutusScriptV1 ->_data-> redeemer -> TxOperationBuilder
txRedeem txin script _data _redeemer =TxOperationBuilder{
    ctxInputs=[ScriptCtxTxIn (script,dataToScriptData _data,dataToScriptData _redeemer,txin)],
    ctxOutputs=[],
    ctxSignatures=[]
  }

-- Redeem from Script Address.
txRedeemUtxo :: (ToData a2, ToData a3) =>
  UTxO AlonzoEra
  -> PlutusScript PlutusScriptV1 -> a2 -> a3 -> TxOperationBuilder
txRedeemUtxo utxo script _data _redeemer =TxOperationBuilder{
    ctxInputs=[ScriptUtxoCtxTxIn (script,dataToScriptData _data,dataToScriptData _redeemer,utxo)],
    ctxOutputs=[],
    ctxSignatures=[]
  }

txAddSignature :: SigningKey PaymentKey -> TxOperationBuilder
txAddSignature skey =TxOperationBuilder{
  ctxInputs= [],
  ctxOutputs=[],
  ctxSignatures=[skey]
}

-- Redeem from Script Address.
txRedeemUtxoWithValidator :: (ToData a2, ToData a3) =>
  UTxO AlonzoEra
  -> Validator -> a2 -> a3 -> TxOperationBuilder
txRedeemUtxoWithValidator utxo validator _data _redeemer =TxOperationBuilder{
    ctxInputs=[ScriptUtxoCtxTxIn (PlutusScriptSerialised  serialisedScript ,dataToScriptData _data,dataToScriptData _redeemer,utxo)],
    ctxOutputs=[],
    ctxSignatures=[]
  }
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



mkTx :: IsNetworkCtx v =>v ->TxOperationBuilder
  -> AddressInEra AlonzoEra
  -> IO TxResult
mkTx networkCtx (TxOperationBuilder input output signature ) walletAddrInEra   = do
  (NetworkContext conn  pParam) <- toNetworkContext networkCtx
  walletAddr <-unMaybe (SomeError "unexpected error converting address to another type") (deserialiseAddress AsAddressAny (serialiseAddress  walletAddrInEra))
  UTxO walletUtxos <- queryUtxos conn walletAddr
  mappedOutput <- mapM (toOuotput (networkCtxNetwork networkCtx)) output
  (operationUtxos,txins) <- resolveAndSumIns conn input
  let operationUtoSum=foldMap txOutValue (Map.elems operationUtxos)
  if not hasScriptInput
  then executeMkBalancedBody pParam  (UTxO walletUtxos) (mkBody  txins mappedOutput TxInsCollateralNone  pParam)  operationUtoSum walletAddrInEra

  else (do
    (FullNetworkContext _  _ systemStart eraHistory ) <- toFullNetworkContext networkCtx
    let possibleCollaterals= Map.filter isOnlyAdaTxOut walletUtxos
    if null possibleCollaterals then throw (SomeError "BuildTx: No utxo usable as collateral") else pure ()
    let (myCollateral,_)= head $ Map.toList possibleCollaterals
    let txInsCollateral=TxInsCollateral CollateralInAlonzoEra  [myCollateral ]

    let bodyRevsion0 =   mkBody txins mappedOutput txInsCollateral pParam
    (TxResult fee usedWalletUtxos balancedRevisionContent0 balancedRevision0)<-case
        mkBalancedBody
          pParam (UTxO walletUtxos) bodyRevsion0 operationUtoSum
          walletAddrInEra
      of
        Left tbe -> throw $ SomeError $ "First Balance :" ++ show tbe
        Right x0 -> pure x0
    let usedUtxos=UTxO (Map.fromList usedWalletUtxos <> operationUtxos)
    let (orderedIns,orderedOuts)=case balancedRevision0 of {
          ShelleyTxBody sbe
          (LedgerBody.TxBody ins _ outs _ _ _ _ _ _ _ _ _ _) scs tbsd m_ad tsv
          -> (Set.map fromShelleyTxIn ins,outs)
    }
    let v= evaluateTransactionExecutionUnits AlonzoEraInCardanoMode systemStart eraHistory pParam usedUtxos balancedRevision0
    modifiedIns<- case v of
      Left tvie -> throw $ SomeError $ "ExecutionUnitCalculation :" ++ show tvie
      Right mp -> applyTxInExecutionUnits (txIns balancedRevisionContent0) orderedIns mp
    executeMkBalancedBody pParam (UTxO walletUtxos)  (mkBody  modifiedIns mappedOutput txInsCollateral pParam)  operationUtoSum walletAddrInEra)

  where
    hasScriptInput = any (\case
                              ScriptCtxTxIn x0 -> True
                              ScriptUtxoCtxTxIn x0 -> True
                              _ -> False )  input
    applyTxInExecutionUnits :: [(TxIn,BuildTxWith  BuildTx (Witness WitCtxTxIn AlonzoEra))]
      -> Data.Set.Set TxIn
      -> Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits)
      -> IO [(TxIn,BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
    applyTxInExecutionUnits ins orderedIns execUnitMap = do
      mapM   doMap (zip [0..] (Set.toList orderedIns))
      where
        insMap=Map.fromList ins
        doMap  (index,txIn)= case Map.lookup txIn insMap of
          Nothing   ->  throw $ SomeError   "Look how they maccasacred my boy"
          Just item -> case Map.lookup (ScriptWitnessIndexTxIn index) execUnitMap of
            Nothing -> pure (txIn,item)
            Just a -> case a of
              Left e -> throw $ SomeError  $ "ResolveExecutionUnit for txin: "++ show e
              Right exUnits ->pure $  case  item of { BuildTxWith wit -> case wit of
                                KeyWitness kwic -> throw $ SomeError  $ "ResolveExecutionUnit for txin: wrong hit "
                                ScriptWitness swic sw -> case sw of
                                  SimpleScriptWitness slie ssv ss -> throw $ SomeError  $ "ResolveExecutionUnit for txin: wrong hit "
                                  PlutusScriptWitness slie psv ps sd sd' eu ->
                                    (
                                        txIn
                                      , BuildTxWith $  ScriptWitness ScriptWitnessForSpending
                                          $ PlutusScriptWitness  slie psv ps sd sd' exUnits
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

    toInput ( inCtx ::TestCtxIn)= case inCtx of
      UtxoCtxIn (UTxO mp) -> Right $  map (\item-> (item,(fst item,BuildTxWith $ KeyWitness KeyWitnessForSpending)))   (Map.toList mp)
      TxInCtxIn ti -> Left (ti,BuildTxWith $ KeyWitness KeyWitnessForSpending)
      ScriptCtxTxIn (script,_data,_redeemer,txin) -> Left (txin,BuildTxWith $  ScriptWitness ScriptWitnessForSpending $ plutusWitness script _data _redeemer defaultExunits)
      ScriptUtxoCtxTxIn (script, _data,_redeemer,UTxO mp) -> Right $ map (\item-> (item,(fst  item,BuildTxWith $  ScriptWitness ScriptWitnessForSpending $ plutusWitness script _data _redeemer defaultExunits)))  (Map.toList mp)

    toOuotput network (outCtx :: TestCtxOut) =  do
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
    defaultExunits=ExecutionUnits 10000 10000
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
  -> Either
      TxBodyError
      TxResult
mkBalancedBody  pParams (UTxO utxoMap)  txbody inputSum walletAddr =
    do
      -- first iteration
      let (inputs1,change1) =minimize txouts startingChange
          txIns1=map utxoToTxBodyIn inputs1
          bodyContent1=modifiedBody (map utxoToTxBodyIn inputs1) change1 startingFee
      txBody1 <- unEither $ case makeTransactionBody bodyContent1 of
        Left tbe -> Left $ SomeError $ show tbe
        Right tb -> Right $ tb

      let modifiedChange1=change1 <> negLovelace  fee1 <> lovelaceToValue startingFee
          fee1= evaluateTransactionFee pParams txBody1 1 0
          (inputs2,change2)= minimize txouts modifiedChange1
          txIns2=map utxoToTxBodyIn inputs2
          bodyContent2 =modifiedBody txIns2 change2 fee1
       -- if selected utxos are  sufficient to pay transaction fees, just use the fee and make txBody
       -- otherwide, reselect txins and recalculate fee. it's very improbable that the we will need more txouts now
      if positiveValue modifiedChange1
        then do
          let  modifiedBody'=(modifiedBody txIns1 modifiedChange1 fee1 )
          txBody<-makeTransactionBody modifiedBody'
          Right (TxResult fee1 inputs1  modifiedBody'  txBody)
        else do
          txbody2 <- makeTransactionBody bodyContent2
          let fee2=evaluateTransactionFee pParams txbody2 1 0
              modifiedChange2= change2 <> negLovelace fee2 <> lovelaceToValue fee1
          if fee2 == fee1
            then Right  (TxResult fee2 inputs2 bodyContent2 txbody2)
            else do
              let body=modifiedBody txIns2 modifiedChange2 fee2
              txBody <- makeTransactionBody body
              Right (TxResult fee2 inputs2 body txBody)

  where
  performBalance  change fee= do
            let (inputs,change') =minimize txouts (change <> negLovelace fee)
                bodyContent=modifiedBody (map utxoToTxBodyIn inputs) change' fee
            txBody1<-makeTransactionBody bodyContent

            let modifiedChange1=change' <> negLovelace  fee' <> lovelaceToValue fee
                fee'= evaluateTransactionFee pParams txBody1 1 0
                (inputs2,change2)= minimize txouts modifiedChange1
                newBody =modifiedBody (map utxoToTxBodyIn inputs2) change2 fee'
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

  utxoToTxBodyIn (txIn,_) =(txIn,BuildTxWith $ KeyWitness KeyWitnessForSpending)

  minimize' utxos remainingChange = (doMap,remainingChange)
    where
      doMap=map (\(txin,txout) -> tobodyIn txin) utxos
      tobodyIn _in=(_in,BuildTxWith $ KeyWitness KeyWitnessForSpending)
      val  out= txOutValueToValue $ txOutValue  out



  -- change is whatever will remain after making payment.
  -- At the beginning, we will assume that we will all the available utxos, 
  -- so it should be a +ve value, otherwise it means we don't have sufficient balance to fulfill the transaction 
  startingChange=(negateValue $ foldMap (txOutValueToValue  . txOutValue ) modifiedOuts )  --already existing outputs
                  <> (if  null (txIns txbody) then mempty else inputSum) -- already existing inputs
                  <> (foldMap (txOutValueToValue  . txOutValue) $  Map.elems utxoMap) -- sum of all the available utxos
                  <> negateValue (lovelaceToValue startingFee)
  utxoToTxOut (UTxO map)=Map.toList map

  txOutValueToValue :: TxOutValue era -> Value
  txOutValueToValue tv =
    case tv of
      TxOutAdaOnly _ l -> lovelaceToValue l
      TxOutValue _ v -> v

  txOutValue (TxOut _ v _) = v
  
  modifiedOuts= map includeMin (txOuts txbody)
  includeMin txOut= case txOut of {TxOut addr v hash-> case v of
                                     TxOutAdaOnly oasie lo -> txOut
                                     TxOutValue masie va ->
                                       if selectAsset va AdaAssetId == Quantity  0
                                       then TxOut  addr (TxOutValue masie (va <> lovelaceToValue  (Lovelace 2_000_000))) hash
                                       else txOut }
  modifiedBody txins change fee= content
    where
      content=(TxBodyContent  {
            txIns=txins ++ txIns txbody,
            txInsCollateral=txInsCollateral txbody,
            txOuts=  if nullValue change
                  then modifiedOuts
                  else modifiedOuts ++ [ TxOut  walletAddr (TxOutValue MultiAssetInAlonzoEra change) TxOutDatumHashNone]  ,
              -- [TxOut alonzoAddr (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ Lovelace 5927107)) TxOutDatumHashNone ],
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
  -> f TxResult
executeMkBalancedBody  pParams utxos  txbody inputSum walletAddr=do
  let balancedBody=mkBalancedBody pParams utxos txbody inputSum walletAddr
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
  let balancedBody = mkBalancedBody pParam  utxos body sum   $  skeyToAddrInEra sKey (localNodeNetworkId conn)

  submitEitherBalancedBody conn  balancedBody sKey

submitEitherBalancedBody ::
  LocalNodeConnectInfo CardanoMode
  ->  Either TxBodyError TxResult
  -> SigningKey PaymentKey
  -> IO TxId
submitEitherBalancedBody conn eitherBalancedBody skey =
      --End Balance transaction body with fee
  case  eitherBalancedBody of
    Left tbe ->
      throw $ SomeError $  "Coding Error : Balanced TxBody has error : " ++  show tbe
    Right (TxResult _ _ _ txBody) ->signAndSubmitTxBody conn txBody skey

