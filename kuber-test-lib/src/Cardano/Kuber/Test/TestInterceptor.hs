{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
module Cardano.Kuber.Test.TestInterceptor where
import Cardano.Api
import  Data.Set (Set)
import Data.Map (Map)
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Crypto
import Cardano.Kuber.Api
import Control.Exception (Exception)
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Data.Foldable as Foldable
import Cardano.Api.Shelley
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Conc (TVar, STM, newTVarIO, atomically, readTVarIO, writeTVar)
import Data.Maybe (mapMaybe)
import Control.Monad.IO.Class (liftIO)
import Cardano.Kuber.Util (addressInEraToAddressAny)
import GHC.Conc.Sync (readTVar)
import qualified Debug.Trace as Debug
import Cardano.Ledger.Api (EraTxBody(inputsTxBodyL))
import Control.Lens ((^.))
import Cardano.Ledger.Babbage.TxBody (outputs')

data KontractState  = KontractState {
    filteredCredentials:: Set (Ledger.PaymentCredential StandardCrypto),
    kUtxoSet :: Map TxIn (TxOut CtxUTxO ConwayEra),
    validTxs:: Set TxId
}

data WithState  api b = WithState  api (TVar b)


data SimpleState = SimpleState

class HasSimpleState  w where
  getSimpleState :: w ->   Kontract api w e w
  updateSimpleState :: w -> Kontract api w e ()

class  HasKontractState holder w  where
  getState  :: Exception e =>   Kontract holder w e w
  updateState :: Exception e=>   w -> Kontract holder w e ()

instance HasKontractState (WithState api b) b where
  getState :: Exception e =>   Kontract (WithState api b) b e b
  getState = do
      (WithState _ tvar ) <- kGetBackend
      liftIO $ readTVarIO tvar

  updateState :: Exception e => b ->  Kontract (WithState api b) b e ()
  updateState  val= do
      (WithState _ tvar ) <- kGetBackend
      liftIO $ atomically $ writeTVar tvar val


data (Exception e)  =>  SubmitInterceptor  api w e  r =
    SubmitInterceptor (w,Kontract api w e r)


updateBackend ::  Exception e => api ->  Kontract api' w e ()  -> Kontract api w e ()
updateBackend a  k=  KLift (\api -> pure$ pure ())

-- data  InterceptedForTest api = InterceptedForTest 

newtype InterceptedForTest api = InterceptedForTest (WithState api  KontractState)


instance HasKontractState ( InterceptedForTest api )  KontractState where
  getState :: ( Exception e) =>
       Kontract (InterceptedForTest api) KontractState e KontractState
  getState = do
      (InterceptedForTest  (WithState api  tvar) ) <- kGetBackend
      liftIO $ readTVarIO tvar

  updateState ::  (Exception e) =>
       KontractState -> Kontract (InterceptedForTest api) KontractState e ()
  updateState  newVal= do
      (InterceptedForTest  (WithState api  tvar) ) <- kGetBackend
      liftIO $ atomically $  writeTVar tvar newVal


-- | Read the txbody and update Utxo state. 
applyTxBody  :: TxBody ConwayEra ->  KontractState -> KontractState
applyTxBody txb ks =  ks {kUtxoSet=newUtxoSet}
  where
    (KontractState  creds stateUtxos vtxs) =  ks
    -- we ignore the byron txbody
    (ShelleyTxBody sbe tb scs tbsd m_ad tsv) = txb
    ins=Set.map fromShelleyTxIn  $ tb ^. inputsTxBodyL 
    outs'=   Foldable.toList $ outputs' tb
    outs= map (fromShelleyTxOut ShelleyBasedEraConway) outs'
    filteredOuts = filter (\(i,TxOut addr val d s) ->
          case addr of {  AddressInEra atie ad -> case ad of
                          ShelleyAddress net cre sr -> cre `elem` creds
                          _ -> False
                      }) utxos
    newUtxoSet =
        Map.filterWithKey (\k v-> k `elem` ins ) stateUtxos
        <> Map.fromList filteredOuts
    txHash= getTxId txb
    toUtxoItem index item = (TxIn txHash (TxIx index),  item)
    utxos = zipWith  toUtxoItem [0..] outs

markTxValid  ::  TxId -> KontractState -> KontractState
markTxValid txid  val@KontractState{validTxs=txs} = val{validTxs =Set.insert  txid txs }

applyUpdate ::  (KontractState -> KontractState)  -> TVar KontractState -> STM ()
applyUpdate f s = do
   state <- readTVar s
   writeTVar s (f state)

applyUpdate' :: (KontractState -> KontractState) ->  Kontract (InterceptedForTest a) w FrameworkError ()
applyUpdate' f = do
  (InterceptedForTest  (WithState api tvar )) <- kGetBackend
  liftIO (atomically $  applyUpdate f tvar)

instance (HasSubmitApi  a) => HasSubmitApi  (InterceptedForTest a )  where
  kSubmitTx :: HasSubmitApi a =>
    Tx ConwayEra -> Kontract (InterceptedForTest a) w FrameworkError ()
  kSubmitTx tx  = do
    (InterceptedForTest  (WithState api tvar )) <- kGetBackend
    applyUpdate' (\k@(KontractState creds map validTxs) ->
        if tid `elem` validTxs
          then applyTxBody  tb k
          else k -- you can't directly submit a transaction without building it with kBuildTx
      )
    pure ()
    where
        tb=getTxBody tx
        tid=getTxId tb

instance (HasKuberAPI a) => HasKuberAPI (InterceptedForTest a ) where
  kTxBuildTxBody :: TxBuilder-> Kontract (InterceptedForTest a) w FrameworkError (TxBody ConwayEra)
  kTxBuildTxBody tb  = do
    result <- forwardBackend kTxBuildTxBody tb
    applyUpdate' ( markTxValid (getTxId result))
    pure result

  kBuildTx       :: TxBuilder -> Kontract (InterceptedForTest a) w FrameworkError (Tx ConwayEra)
  kBuildTx  tb= do
    result <- forwardBackend kBuildTx tb
    applyUpdate' ( markTxValid (getTxId $ getTxBody result))
    pure result

  kTimeToSlot           :: POSIXTime -> Kontract (InterceptedForTest a) w FrameworkError SlotNo
  kTimeToSlot = forwardBackend kTimeToSlot

  kSlotToTime           ::  SlotNo    -> Kontract (InterceptedForTest a)  w FrameworkError POSIXTime
  kSlotToTime = forwardBackend kSlotToTime

  kEvaluateExUnits :: Tx ConwayEra -> Kontract (InterceptedForTest a)  w FrameworkError (Map ScriptWitnessIndex (Either FrameworkError ExecutionUnits))
  kEvaluateExUnits = forwardBackend kEvaluateExUnits

  kCalculateMinFee :: Tx ConwayEra -> Kontract (InterceptedForTest a)  w FrameworkError  Lovelace
  kCalculateMinFee = forwardBackend  kCalculateMinFee

  kBuildAndSubmit :: TxBuilder -> Kontract (InterceptedForTest a) w FrameworkError (Tx ConwayEra)
  kBuildAndSubmit tb = do
    result <- forwardBackend kBuildTx tb
    applyUpdate' ( applyTxBody ( getTxBody result))
    pure result


instance (HasChainQueryAPI a) => HasChainQueryAPI ( InterceptedForTest a) where
  kGetNetworkId  = forwardQuery   kGetNetworkId
  kQueryProtocolParams = forwardQuery kQueryProtocolParams
  kQuerySystemStart = forwardQuery kQuerySystemStart
  kQueryGenesisParams = forwardQuery kQueryGenesisParams
  kQueryUtxoByAddress addrs= do
    (InterceptedForTest  (WithState api state  )) <- kGetBackend
    (KontractState _ utxos _)<- liftIO $ atomically $ readTVar state
    pure $  UTxO $ Map.filterWithKey (\k (TxOut addr _ _ _ )  -> addressInEraToAddressAny addr `elem` addrs )  utxos

  kQueryUtxoByTxin tin = do
    (InterceptedForTest  (WithState api state  )) <- kGetBackend
    (KontractState _ utxos _)<- liftIO $ atomically $ readTVar state
    pure $  UTxO $ Map.filterWithKey (\k v  -> k `elem` tin )  utxos

  kQueryChainPoint = forwardQuery kQueryChainPoint


forwardBackend :: (Exception e) => (t -> Kontract a w e r) -> t -> Kontract (InterceptedForTest a) w e r
forwardBackend  f p1 = do
  (InterceptedForTest  (WithState api tvar)) <- kGetBackend
  result <-liftIO $ evaluateKontract api (f p1)
  case result of
    Left e -> KError e
    Right r -> pure r

forwardQuery :: (Exception e) =>Kontract a w e r -> Kontract (InterceptedForTest a) w e  r
forwardQuery  f  = do
  (InterceptedForTest  (WithState api tvar  )) <- kGetBackend
  result <- liftIO $ evaluateKontract api f
  case result of
    Left e -> KError e
    Right r -> pure r

withInterceptorForTest :: [AddressInEra ConwayEra] -> a -> IO  (InterceptedForTest a)
withInterceptorForTest addrs  = withInterceptorForTest' creds
  where
    creds :: [Address ShelleyAddr]
    creds = mapMaybe  (\(AddressInEra atie ad) -> case ad of
                         add@(ShelleyAddress net cre sr) -> Just  add
                         _ -> Nothing )  addrs

withInterceptorForTest' :: [Address ShelleyAddr] -> a -> IO  (InterceptedForTest a)
withInterceptorForTest' addrs backend = do
  var <- newTVarIO $ KontractState (Set.fromList creds) mempty mempty
  pure $ InterceptedForTest (WithState backend var  )
  where
    creds = map  (\(ShelleyAddress net cre sr) -> cre )  addrs