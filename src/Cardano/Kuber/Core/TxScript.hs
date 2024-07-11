{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Cardano.Kuber.Core.TxScript where
import Cardano.Api
    
data TxSimpleScript = TxSimpleScript SimpleScript
                            deriving(Show)

data TxScript = TxScriptSimple TxSimpleScript
        |   TxScriptPlutus   TxPlutusScript
        deriving (Show)


data  TxPlutusScript  =
    TxPlutusScriptV1 (PlutusScript  PlutusScriptV1)
  | TxPlutusScriptV2 (PlutusScript  PlutusScriptV2)
  | TxPlutusScriptV3 (PlutusScript  PlutusScriptV3)
                          deriving (Show)

class IsPlutusVersion v where
  toTxPlutusScriptInstance :: PlutusScript v -> TxPlutusScript

instance IsPlutusVersion PlutusScriptV1  where
   toTxPlutusScriptInstance = TxPlutusScriptV1

instance IsPlutusVersion PlutusScriptV2  where
   toTxPlutusScriptInstance = TxPlutusScriptV2

instance IsPlutusVersion PlutusScriptV3  where
   toTxPlutusScriptInstance = TxPlutusScriptV3


class IsPlutusScript sc where
  toTxPlutusScript :: sc -> TxPlutusScript

class IsSimpleScript sc where
  toTxSimpleScript :: sc -> TxSimpleScript

instance IsPlutusScript TxPlutusScript where
  toTxPlutusScript = id

instance IsSimpleScript TxSimpleScript where
  toTxSimpleScript = id

instance  IsSimpleScript SimpleScript  where
  toTxSimpleScript  = toTxSimpleScriptInstance

instance IsSimpleScript (Script ver) where
  toTxSimpleScript  (Cardano.Api.SimpleScript  sc) = toTxSimpleScriptInstance sc
  toTxSimpleScript _ = error "Impossible"


hashPlutusScript :: TxPlutusScript -> ScriptHash
hashPlutusScript sc = case sc of
  TxPlutusScriptV1 ps -> hashScript (PlutusScript  PlutusScriptV1 ps)
  TxPlutusScriptV2 ps -> hashScript (PlutusScript  PlutusScriptV2 ps)
  TxPlutusScriptV3 ps -> hashScript (PlutusScript  PlutusScriptV3 ps)

plutusScriptAddr :: TxPlutusScript -> NetworkId -> AddressInEra ConwayEra
plutusScriptAddr sc networkId =
    let payCred = PaymentCredentialByScript (hashPlutusScript sc)
        addr = makeShelleyAddress networkId payCred NoStakeAddress
        addrInEra = AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) addr
    in addrInEra

plutusScriptToScriptAny :: TxPlutusScript -> ScriptInAnyLang
plutusScriptToScriptAny sc = case sc of
    TxPlutusScriptV1 ps -> ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV1) (PlutusScript PlutusScriptV1 ps)
    TxPlutusScriptV2 ps -> ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV2) (PlutusScript PlutusScriptV2 ps)
    TxPlutusScriptV3 ps -> ScriptInAnyLang (PlutusScriptLanguage  PlutusScriptV3) (PlutusScript PlutusScriptV3 ps)

instance (IsPlutusVersion ver =>  IsPlutusScript (PlutusScript ver)) where
  toTxPlutusScript  = toTxPlutusScriptInstance

instance (IsPlutusVersion ver =>  IsPlutusScript (Script ver)) where
  toTxPlutusScript  (PlutusScript psv ps) = toTxPlutusScript ps
  toTxPlutusScript _ = error "Impossible"


class IsMintingScript sc where
  toTxMintingScript:: sc -> TxScript

txScriptPolicyId :: TxScript -> PolicyId
txScriptPolicyId sc = PolicyId (hashTxScript sc)


txScriptAddress :: TxScript -> NetworkId ->  StakeAddressReference  -> AddressInEra ConwayEra
txScriptAddress sc net = makeShelleyAddressInEra net   (PaymentCredentialByScript $ txScriptHash sc)

txScriptHash :: TxScript -> ScriptHash
txScriptHash = hashTxScript

hashTxScript :: TxScript -> ScriptHash
hashTxScript sc = case sc of
  TxScriptSimple tss -> hashScript $ SimpleScript SimpleScriptV1 ss
  TxScriptPlutus tps -> hashPlutusScript tps



txScriptToScriptAny :: TxScript -> ScriptInAnyLang
txScriptToScriptAny sc = case sc of
  TxScriptSimple tss ->  ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1) (SimpleScript SimpleScriptV1 ss)
    -- TxSimpleScriptV1 ss -> 
    -- TxSimpleScriptV2 ss -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2) (SimpleScript SimpleScriptV2 ss)
  TxScriptPlutus tps -> plutusScriptToScriptAny tps

 -- TxPlutusMintingScript tps -> plutusScriptToScriptAny tps


class IsScriptVersion v where
  translationFunc :: Script v -> TxScript

instance IsScriptVersion PlutusScriptV1  where
  translationFunc (PlutusScript psv ps)= TxScriptPlutus $ toTxPlutusScript  ps

instance IsScriptVersion PlutusScriptV2  where
  translationFunc (PlutusScript psv ps)= TxScriptPlutus $ toTxPlutusScript  ps

instance IsScriptVersion PlutusScriptV3  where
  translationFunc (PlutusScript psv ps)= TxScriptPlutus $ toTxPlutusScript  ps
instance IsScriptVersion v  => IsMintingScript (Script v) where
  toTxMintingScript sc = translationFunc sc


instance  IsMintingScript SimpleScript  where
  toTxMintingScript = toTxMintingScriptInstance

instance (IsPlutusVersion ver =>  IsMintingScript (PlutusScript ver)) where
  toTxMintingScript  v = TxScriptPlutus (toTxPlutusScript v)
