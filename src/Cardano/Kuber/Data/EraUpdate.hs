{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Cardano.Kuber.Data.EraUpdate where
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra (..))
import Cardano.Api
import Cardano.Api.Shelley
import qualified Data.Map as Map
import Cardano.Ledger.Api (downgradePParams)

updateUtxoEra :: IsTxBuilderEra era => UTxO era1 -> UTxO era
updateUtxoEra (UTxO mp) = UTxO $ updateuMapEra mp

updateuMapEra :: IsTxBuilderEra era => Map.Map k (TxOut ctx era1) -> Map.Map k (TxOut ctx era)
updateuMapEra = Map.map updateTxOutInEra

updateTxOutInEra :: IsTxBuilderEra era => TxOut ctx era1 -> TxOut ctx era
updateTxOutInEra txout = case txout of TxOut aie tov tod rs -> TxOut (updateAddressEra aie) (updateToutValue tov) (updateTxOutDatum tod) (updateRefScript rs)

updateTxOutInEra' :: IsTxBuilderEra era => TxOut CtxTx era1 -> TxOut CtxTx era
updateTxOutInEra' txout = case txout of TxOut aie tov tod rs -> TxOut (updateAddressEra aie) (updateToutValue tov) (updateTxOutDatum' tod) (updateRefScript rs)

updateRefScript :: IsTxBuilderEra era => ReferenceScript era1 -> ReferenceScript era
updateRefScript = \case
  ReferenceScript beo sial -> ReferenceScript bBabbageOnward sial
  ReferenceScriptNone -> ReferenceScriptNone

updateTxOutDatum' :: IsTxBuilderEra era => TxOutDatum CtxTx era1 -> TxOutDatum CtxTx era
updateTxOutDatum' datum = case datum of
  TxOutDatumNone -> TxOutDatumNone
  TxOutDatumHash aeo ha -> TxOutDatumHash bAlonzoOnward ha
  TxOutDatumInline beo hsd -> TxOutDatumInline bBabbageOnward hsd
  TxOutDatumInTx aeo hsd -> TxOutDatumInTx bAlonzoOnward hsd
  _ -> error "Cardano.Kuber.Core.Data.EraUpdate.updateTxOutDatum' : Impossible"

updateTxOutDatum :: IsTxBuilderEra era => TxOutDatum ctx era1 -> TxOutDatum ctx era
updateTxOutDatum datum = case datum of
  TxOutDatumNone -> TxOutDatumNone
  TxOutDatumHash aeo ha -> TxOutDatumHash bAlonzoOnward ha
  TxOutDatumInline beo hsd -> TxOutDatumInline bBabbageOnward hsd
  _ -> error "Cardano.Kuber.Core.Data.EraUpdate.updateTxOutDatum : Impossible"

updateToutValue :: IsTxBuilderEra era => TxOutValue era1 -> TxOutValue era
updateToutValue val = case val of
  TxOutAdaOnly btae lo -> TxOutValue bMaryOnward (lovelaceToValue lo)
  TxOutValue meo va -> TxOutValue bMaryOnward va

updateAddressEra :: IsTxBuilderEra era => AddressInEra era1 -> AddressInEra era
updateAddressEra addr = addrInfo
  where
    addrInfo = case addr of
      AddressInEra atie ad -> case atie of
        ByronAddressInAnyEra -> AddressInEra ByronAddressInAnyEra ad
        ShelleyAddressInEra sbe' -> AddressInEra (ShelleyAddressInEra bShelleyBasedEra) ad

updatePParamEra :: CardanoEra era -> LedgerProtocolParameters ConwayEra -> LedgerProtocolParameters era
updatePParamEra cera ulP@(LedgerProtocolParameters pparam) = case cera of
  BabbageEra -> (LedgerProtocolParameters (downgradePParams () pparam) :: LedgerProtocolParameters BabbageEra)
  ConwayEra -> ulP
  _ -> error "Unexpected"
