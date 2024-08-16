{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
module Cardano.Kuber.Core.TxScript where
import Cardano.Api
import Cardano.Api.Shelley (PlutusScript(..))
import qualified Data.ByteString.Short as SBS

data TxSimpleScript = TxSimpleScript SimpleScript
                            deriving(Show)

data TxScript = TxScriptSimple SimpleScript
        |   TxScriptPlutus   TxPlutusScript
        deriving (Show)


data  TxPlutusScript  =
    TxPlutusScriptV1 (PlutusScript  PlutusScriptV1)
  | TxPlutusScriptV2 (PlutusScript  PlutusScriptV2)
  | TxPlutusScriptV3 (PlutusScript  PlutusScriptV3)
                          deriving (Show)

class IsPlutusVersion v where
  toTxPlutusScriptInstance :: PlutusScript v -> TxPlutusScript

instance IsPlutusVersion PlutusScriptV1 where
  toTxPlutusScriptInstance = TxPlutusScriptV1

instance IsPlutusVersion PlutusScriptV2 where
  toTxPlutusScriptInstance = TxPlutusScriptV2

instance IsPlutusVersion PlutusScriptV3 where
  toTxPlutusScriptInstance = TxPlutusScriptV3  

class IsPlutusScript sc where
  toTxPlutusScript :: sc -> TxPlutusScript

class IsSimpleScript sc where
  toTxSimpleScript :: sc -> TxSimpleScript

instance IsPlutusScript TxPlutusScript where
  toTxPlutusScript = id

instance IsSimpleScript TxSimpleScript where
  toTxSimpleScript = id

instance IsSimpleScript SimpleScript where
  toTxSimpleScript = TxSimpleScript

hashPlutusScript :: TxPlutusScript -> ScriptHash
hashPlutusScript sc = case sc of
  TxPlutusScriptV1 ps -> hashScript (PlutusScript PlutusScriptV1 ps)
  TxPlutusScriptV2 ps -> hashScript (PlutusScript PlutusScriptV2 ps)
  TxPlutusScriptV3 ps -> hashScript (PlutusScript PlutusScriptV3 ps)

plutusScriptAddr :: IsShelleyBasedEra era => TxPlutusScript -> NetworkId -> AddressInEra era
plutusScriptAddr sc networkId =
  let payCred = PaymentCredentialByScript (hashPlutusScript sc)
      addr = makeShelleyAddress networkId payCred NoStakeAddress
      addrInEra = AddressInEra (ShelleyAddressInEra shelleyBasedEra) addr
   in addrInEra

plutusScriptToScriptAny :: TxPlutusScript -> ScriptInAnyLang
plutusScriptToScriptAny sc = case sc of
  TxPlutusScriptV1 ps -> ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) (PlutusScript PlutusScriptV1 ps)
  TxPlutusScriptV2 ps -> ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) (PlutusScript PlutusScriptV2 ps)
  TxPlutusScriptV3 ps -> ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) (PlutusScript PlutusScriptV3 ps)

instance IsPlutusVersion ver => IsPlutusScript (PlutusScript ver) where
  toTxPlutusScript = toTxPlutusScriptInstance

-- instance  IsPlutusVersion ver => IsPlutusScript (PlutusScript ver) where
--   toTxPlutusScript  = toTxPlutusScriptInstance

instance IsPlutusVersion ver => IsPlutusScript (Script ver) where
  toTxPlutusScript (PlutusScript psv ps) = toTxPlutusScript ps
  toTxPlutusScript _ = error "Impossible"

class IsMintingScript sc where
  toTxMintingScript :: sc -> TxScript

txScriptPolicyId :: TxScript -> PolicyId
txScriptPolicyId sc = PolicyId (hashTxScript sc)

txScriptAddress :: IsShelleyBasedEra era => TxScript -> NetworkId -> StakeAddressReference -> AddressInEra era
txScriptAddress sc net = makeShelleyAddressInEra shelleyBasedEra net (PaymentCredentialByScript $ txScriptHash sc)

txScriptHash :: TxScript -> ScriptHash
txScriptHash = hashTxScript

hashTxScript :: TxScript -> ScriptHash
hashTxScript sc = case sc of
  TxScriptSimple ss -> hashScript (SimpleScript ss)
  TxScriptPlutus tps -> hashPlutusScript tps

txScriptToScriptAny :: TxScript -> ScriptInAnyLang
txScriptToScriptAny sc = case sc of
  TxScriptSimple  ss -> ScriptInAnyLang SimpleScriptLanguage (SimpleScript ss)
  TxScriptPlutus tps -> plutusScriptToScriptAny tps

txScriptFromScriptAny :: ScriptInAnyLang -> TxScript
txScriptFromScriptAny = \case
  ScriptInAnyLang sl sc -> case sc of
    SimpleScript ss -> TxScriptSimple  ss
    PlutusScript psv ps -> case psv of
      PlutusScriptV1 -> TxScriptPlutus $ toTxPlutusScript ps
      PlutusScriptV2 -> TxScriptPlutus $ toTxPlutusScript ps
      PlutusScriptV3 -> TxScriptPlutus $ toTxPlutusScript ps
class IsScriptVersion v where
  translationFunc :: Script v -> TxScript

instance IsScriptVersion PlutusScriptV1 where
  translationFunc (PlutusScript psv ps) = TxScriptPlutus $ toTxPlutusScript ps

instance IsScriptVersion PlutusScriptV2 where
  translationFunc (PlutusScript psv ps) = TxScriptPlutus $ toTxPlutusScript ps

instance IsScriptVersion PlutusScriptV3 where
  translationFunc (PlutusScript psv ps) = TxScriptPlutus $ toTxPlutusScript ps

instance (IsPlutusVersion ver) => IsMintingScript (PlutusScript ver) where
  toTxMintingScript v = TxScriptPlutus (toTxPlutusScript v)

txScriptByteSize :: TxScript -> Int
txScriptByteSize txScript = 
    case txScript of 
      TxScriptPlutus ps -> case ps of 
        TxPlutusScriptV1 (PlutusScriptSerialised sc) -> SBS.length sc
        TxPlutusScriptV2 (PlutusScriptSerialised sc) -> SBS.length sc
        TxPlutusScriptV3 (PlutusScriptSerialised sc) -> SBS.length sc
      TxScriptSimple _ss -> 1 -- TODO: what is the script size in case of simple script?