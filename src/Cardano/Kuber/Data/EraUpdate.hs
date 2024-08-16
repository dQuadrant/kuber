{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardano.Kuber.Data.EraUpdate where

import Cardano.Api
import qualified Cardano.Api.Ledger as L
import Cardano.Api.Shelley
import Cardano.Kuber.Core.TxBuilder (IsTxBuilderEra (..))
import Cardano.Ledger.Api (downgradePParams)
import qualified Data.Map as Map

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

updateToutValue' :: CardanoEra era -> TxOutValue srcEra ->  TxOutValue era
updateToutValue' era toutVal = case era of 
  MaryEra -> TxOutValueShelleyBased ShelleyBasedEraMary (toLedgerValue MaryEraOnwardsMary val)
  AlonzoEra -> TxOutValueShelleyBased ShelleyBasedEraAlonzo (toLedgerValue MaryEraOnwardsAlonzo val)
  BabbageEra -> TxOutValueShelleyBased ShelleyBasedEraBabbage (toLedgerValue MaryEraOnwardsBabbage val)
  ConwayEra ->  TxOutValueShelleyBased ShelleyBasedEraConway (toLedgerValue MaryEraOnwardsConway val)
  _ -> error "Unexpected"

  where
    val=txOutValueToValue toutVal

updateToutValue :: ( IsTxBuilderEra dstEra) => TxOutValue srcEra -> TxOutValue dstEra
updateToutValue  val =  updateToutValue' bCardanoEra val


-- case val of
-- TxOutValueByron lo -> TxOutValueShelleyBased bShelleyBasedEra (toLedgerValue bMaryOnward (lovelaceToValue lo))
-- TxOutValueShelleyBased sbe va -> TxOutValueShelleyBased bShelleyBasedEra (toLedgerValue bMaryOnward (fromLedgerValue sbe va))

-- TxOutAdaOnly btae lo -> TxOutValue bMaryOnward (lovelaceToValue lo)
-- TxOutValue meo va -> TxOutValue bMaryOnward va

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
