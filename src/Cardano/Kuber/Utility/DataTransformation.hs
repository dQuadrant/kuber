{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.Kuber.Utility.DataTransformation where

import Cardano.Api
import Cardano.Api.Shelley (Address (ShelleyAddress), StakeCredential (StakeCredentialByKey, StakeCredentialByScript), fromPlutusData, fromShelleyAddr, fromShelleyPaymentCredential, fromShelleyStakeReference, shelleyPayAddrToPlutusPubKHash)
import qualified Cardano.Api.Shelley as Shelley
import Cardano.Kuber.Data.Parsers (parseAddress)
import qualified Cardano.Ledger.Plutus.TxInfo as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Shelley.API (Credential, Ptr (Ptr), StakeReference (StakeRefNull, StakeRefPtr))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.String (fromString)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass), adaSymbol, adaToken, assetClass, flattenValue, assetClassValue)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), PubKeyHash (PubKeyHash, getPubKeyHash), ToData, TokenName (TokenName), fromBuiltin, toBuiltin, toData, TxOutRef (TxOutRef))
import qualified PlutusLedgerApi.V2 as Plutus

-- | Create enterprise address from SignKey
skeyToAddr :: SigningKey PaymentKey -> NetworkId -> Shelley.Address ShelleyAddr
skeyToAddr skey network =
  makeShelleyAddress network credential NoStakeAddress
  where
    credential = PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey skey

-- | Create enterprise  address from SignKey
skeyToAddrInEra :: IsShelleyBasedEra era => SigningKey PaymentKey -> NetworkId -> AddressInEra era
skeyToAddrInEra skey network = makeShelleyAddressInEra shelleyBasedEra network credential NoStakeAddress
  where
    credential = PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey skey

addressInEraToAddressAny :: AddressInEra era -> AddressAny
addressInEraToAddressAny addr = case addr of AddressInEra atie ad -> toAddressAny ad

sKeyToPkh :: SigningKey PaymentKey -> PubKeyHash
sKeyToPkh skey = PubKeyHash (toBuiltin $ serialiseToRawBytes vkh)
  where
    vkh = verificationKeyHash $ getVerificationKey skey

fromLedgerAddress la = (fromShelleyAddr shelleyBasedEra la)

-- | Convert Pkh (plutus type) to Hash PaymentKey (cardano-api type)
pkhToPaymentKeyHash :: PubKeyHash -> Maybe (Hash PaymentKey)
pkhToPaymentKeyHash pkh = case deserialiseFromRawBytes (AsHash AsPaymentKey) $
  fromBuiltin $
    getPubKeyHash pkh of
  Left _ -> Nothing
  Right ha -> Just ha

-- | Convert skey (plutus type) to Hash PaymentKey (cardano-api type)
skeyToPaymentKeyHash :: SigningKey PaymentKey -> Hash PaymentKey
skeyToPaymentKeyHash skey = verificationKeyHash $ getVerificationKey skey

-- | Convert AddressInEra (cardano-api type) to Hash PaymentKey (cardano-api type).
-- Will return `Nothing` if address is  an Byron Address
addressInEraToPaymentKeyHash :: AddressInEra era -> Maybe (Hash PaymentKey)
addressInEraToPaymentKeyHash a = case a of
  AddressInEra atie ad -> case ad of
    ShelleyAddress net cre sr -> case fromShelleyPaymentCredential cre of
      PaymentCredentialByKey ha -> Just ha
      PaymentCredentialByScript sh -> Nothing
    _ -> Nothing


-- | convert PubKeyhash (plutus tupe) to corresponding Enterprise address (cardano-api type).
-- Note that the transformation  Address <-> Pkh is not symmetrical for all addresses
-- It's symmetrical for Enterprise addresses (because enterprise addresses have no stake Key in it)
pkhToMaybeAddr :: IsShelleyBasedEra era => NetworkId -> PubKeyHash -> Maybe (AddressInEra era)
pkhToMaybeAddr network (PubKeyHash pkh) = do
  key <- vKey
  Just $ makeShelleyAddressInEra shelleyBasedEra network (PaymentCredentialByKey key) NoStakeAddress
  where
    paymentCredential _vkey = PaymentCredentialByKey _vkey
    vKey = case deserialiseFromRawBytes (AsHash AsPaymentKey) $ fromBuiltin pkh of
      Left _ -> Nothing
      Right ha -> Just ha

-- | Convert Shelley Address (cardano-api type) to corresponding PubKeyHash (plutus type)
-- Note that the transformation  Address <-> Pkh is not symmetrical for all addresses
-- It's symmetrical for Enterprise addresses (because enterprise addresses have no stake Key in it)
addrToMaybePkh :: Cardano.Api.Shelley.Address ShelleyAddr -> Maybe PubKeyHash
addrToMaybePkh = shelleyPayAddrToPlutusPubKHash

-- | Convert AddressInEra (cardano-api type) data type  to corresponding ScriptHash.
-- Note that the transformation  Address <-> Pkh is not symmetrical for all addresses
-- It's symmetrical for Enterprise addresses (because enterprise addresses have no stake Key in it).
-- Returns Nothing if Address is Byron era address or if it's a PublicKey Addresss.
addrInEraToPkh :: MonadFail m => AddressInEra e -> m Plutus.PubKeyHash
addrInEraToPkh a = case a of
  AddressInEra atie ad -> case ad of
    a@(ShelleyAddress net cre sr) -> case addrToMaybePkh a of
      Nothing -> fail "Expected PublicKey address got Script Address"
      Just pkh -> pure pkh
    _ -> fail "Byron address is not supported"


addrInEraToValHash :: MonadFail m => AddressInEra e -> m Plutus.ScriptHash
addrInEraToValHash a = case a of
  AddressInEra atie ad -> case ad of
    a@(ShelleyAddress net cre sr) ->
      let credential = toPlutusCredential cre
       in case credential of
            Plutus.PubKeyCredential pkh -> fail "Expected Script address got PublicKey Address"
            Plutus.ScriptCredential vh -> pure vh
    _ -> fail "Byron address is not supported"


-- | Convert the address to Enterprise Address.
-- Enterprise address is an address having no stakeKey. Returns same address if the address is a Byron era address.
unstakeAddr :: IsShelleyBasedEra era => AddressInEra era -> AddressInEra era
unstakeAddr a = case a of
  AddressInEra atie ad -> case ad of
    ShelleyAddress net cre sr -> shelleyAddressInEra shelleyBasedEra $ ShelleyAddress net cre Cardano.Ledger.Shelley.API.StakeRefNull
    _                        -> a


-- | Create Plutus library AssetClass structure from Cardano.Api's AssetId
toPlutusAssetClass :: AssetId -> AssetClass
toPlutusAssetClass (AssetId (PolicyId hash) (AssetName name)) = AssetClass (CurrencySymbol $ toBuiltin $ serialiseToRawBytes hash, TokenName $ toBuiltin name)
toPlutusAssetClass AdaAssetId = AssetClass (CurrencySymbol $ fromString "", TokenName $ fromString "")

-- | Convert (ToData) (i.e. Plutus data) to Cardano.Api's ScriptData structure
dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData = fromPlutusData $ toData sData

-- toPlutusScriptHash :: Cardano.Ledger.Shelley.API.ScriptHash c -> Plutus.ValidatorHash
-- toPlutusScriptHash = Alonzo.transScriptHash

-- | Convert Credential (cardano-api type)  to Credential (plutus type)
toPlutusCredential :: Cardano.Ledger.Shelley.API.Credential keyrole crypto -> Plutus.Credential
toPlutusCredential = Alonzo.transCred

-- | Extract Payment Credential from Shelley Address (cardano-api type) and return  Plutus Credential (plutus type)
addressToPlutusCredential :: Cardano.Api.Shelley.Address ShelleyAddr -> Plutus.Credential
addressToPlutusCredential (ShelleyAddress net cre sr) = toPlutusCredential cre

-- | Get Network Id from address
addressNetworkId :: Cardano.Api.Shelley.Address ShelleyAddr -> NetworkId
addressNetworkId (ShelleyAddress net cre sr) = case net of
  Ledger.Testnet -> Cardano.Api.Testnet (NetworkMagic 1)
  Ledger.Mainnet -> Cardano.Api.Mainnet

-- | Convert Shelley Address (cardano-api type) to Address (plutus type)
toPlutusAddress :: Address ShelleyAddr -> Plutus.Address
toPlutusAddress (ShelleyAddress net cre sr) = Plutus.Address (toPlutusCredential cre) (Alonzo.transStakeReference sr)

-- | Convert  AddressInEra (cardano-api type) to Address (plutus type).It calls `error` internally if the provided address
-- is byron address
addrInEraToPlutusAddress :: AddressInEra era -> Plutus.Address
addrInEraToPlutusAddress addr = case addr of
  AddressInEra atie ad -> case ad of
    ShelleyAddress net cre sr -> Plutus.Address (toPlutusCredential cre) (Alonzo.transStakeReference sr)
    _  -> error "addrInEraToPlutusAddress(): got byron address"

-- | Convert Address (plutus type) to Shelley Address (cardano-api type).
-- Address is re-deserialized, which might fail. This is because it's possible to set arbitary byteString as hashes in plutus.
fromPlutusAddress :: NetworkId -> Plutus.Address -> Maybe (Address ShelleyAddr)
fromPlutusAddress network (Plutus.Address cre m_sc) = makeShelleyAddress network <$> paymentCre <*> stakingCre
  where
    paymentCre = case cre of
      Plutus.PubKeyCredential (PubKeyHash pkh) ->
        ( case deserialiseFromRawBytes (AsHash AsPaymentKey) (fromBuiltin pkh) of
            Left sarbe -> Nothing
            Right ha -> Just ha
        )
          <&> PaymentCredentialByKey
      Plutus.ScriptCredential (Plutus.ScriptHash vh) ->
        ( case deserialiseFromRawBytes AsScriptHash (fromBuiltin vh) of
            Left sarbe -> Nothing
            Right sh -> Just sh
        )
          <&> PaymentCredentialByScript
    stakingCre = case m_sc of
      Nothing -> Just NoStakeAddress
      Just sc -> case sc of
        Plutus.StakingHash cre' -> case cre' of
          Plutus.PubKeyCredential (PubKeyHash pkh) ->
            ( case deserialiseFromRawBytes (AsHash AsStakeKey) (fromBuiltin pkh) of
                Left _ -> Nothing
                Right ha -> Just ha
            )
              <&> StakeCredentialByKey
              <&> StakeAddressByValue
          Plutus.ScriptCredential (Plutus.ScriptHash vh) ->
            ( case deserialiseFromRawBytes AsScriptHash (fromBuiltin vh) of
                Left _ -> Nothing
                Right sh -> Just sh
            )
              <&> StakeCredentialByScript
              <&> StakeAddressByValue
        Plutus.StakingPtr n i j -> Just $ fromShelleyStakeReference $ Cardano.Ledger.Shelley.API.StakeRefPtr (Cardano.Ledger.Shelley.API.Ptr (SlotNo $ fromInteger n) (Ledger.TxIx $ fromInteger i) (Ledger.CertIx $ fromInteger j))

plutusAssetClassToAssetId :: AssetClass -> AssetId
plutusAssetClassToAssetId plutusAssetClass =
  if plutusAssetClass == assetClass adaSymbol adaToken
    then AdaAssetId
    else case plutusAssetClass of
      AssetClass (CurrencySymbol bbs, TokenName bbs') ->
        AssetId
          ( case deserialiseFromRawBytes AsPolicyId (fromBuiltin bbs) of
              Left sarbe -> error "plutusAssetClassToAssetId(): invalid currency symbol"
              Right pi -> pi
          )
          ( case deserialiseFromRawBytes AsAssetName (fromBuiltin bbs') of
              Left sarbe -> error "plutusAssetClassToAssetId(): invalid tokenName"
              Right an -> an
          )

fromPlutusValue :: Plutus.Value -> Value
fromPlutusValue val = mconcat $ map (\(cs, tn, n) -> valueFromList [(plutusAssetClassToAssetId (assetClass cs tn), Quantity n)]) (flattenValue val)

toPlutusValue :: Value -> Plutus.Value
toPlutusValue val1 = mconcat $ Prelude.map (\(asset, Quantity amount) -> assetClassValue (toPlutusAssetClass asset) amount) (valueToList val1)

toPlutusTxOutRef ::  TxIn -> TxOutRef
toPlutusTxOutRef (TxIn tid (TxIx index)) = TxOutRef (Plutus.TxId $ toBuiltin $ serialiseToRawBytes tid) (toInteger index)   
