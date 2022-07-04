{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Cardano.Kuber.Utility.DataTransformation
where
import Cardano.Api
import qualified Cardano.Api.Shelley as Shelley
import Cardano.Api.Byron (Address(ByronAddress))
import Cardano.Api.Shelley (Address(ShelleyAddress), fromPlutusData, fromShelleyPaymentCredential, shelleyPayAddrToPlutusPubKHash, StakeCredential (StakeCredentialByKey, StakeCredentialByScript), fromShelleyStakeReference, Hash (PaymentKeyHash))
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash, getPubKeyHash), ToData, CurrencySymbol (CurrencySymbol), TokenName (TokenName), toBuiltin, toData, fromBuiltin)
import Data.ByteString
import qualified Cardano.Binary as Cborg
import Cardano.Ledger.Shelley.API (Credential(KeyHashObj, ScriptHashObj), StakeReference (StakeRefNull, StakeRefBase, StakeRefPtr), ScriptHash (ScriptHash), Ptr (Ptr))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import Data.String (fromString)
import Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.BaseTypes  as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import Data.Functor ((<&>))




-- Create enterprise address from SignKey
skeyToAddr:: SigningKey PaymentKey -> NetworkId -> Shelley.Address ShelleyAddr
skeyToAddr skey network =
  makeShelleyAddress  network  credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey

-- Create enterprise  (AddressInEra BabbageEra) datastructure
skeyToAddrInEra ::  SigningKey PaymentKey -> NetworkId -> AddressInEra BabbageEra
skeyToAddrInEra skey network=makeShelleyAddressInEra network   credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey

addressInEraToAddressAny :: AddressInEra era -> AddressAny
addressInEraToAddressAny addr = case addr of { AddressInEra atie ad -> toAddressAny ad }



sKeyToPkh:: SigningKey PaymentKey -> PubKeyHash
sKeyToPkh skey= PubKeyHash (toBuiltin  $  serialiseToRawBytes  vkh)
  where
    vkh=verificationKeyHash   $ getVerificationKey  skey

pkhToPaymentKeyHash :: PubKeyHash -> Maybe  (Hash PaymentKey )
pkhToPaymentKeyHash pkh = deserialiseFromRawBytes (AsHash AsPaymentKey) $ fromBuiltin $ getPubKeyHash  pkh

skeyToPaymentKeyHash :: SigningKey PaymentKey -> Hash PaymentKey
skeyToPaymentKeyHash skey = verificationKeyHash   $ getVerificationKey  skey

addressInEraToPaymentKeyHash :: AddressInEra BabbageEra -> Maybe (Hash PaymentKey)
addressInEraToPaymentKeyHash a = case a of { AddressInEra atie ad -> case ad of
                                               ByronAddress ad' -> Nothing
                                               ShelleyAddress net cre sr -> case fromShelleyPaymentCredential cre of
                                                 PaymentCredentialByKey ha -> Just ha
                                                 PaymentCredentialByScript sh -> Nothing
                                    }

-- convert PubKeyhash to corresponding Enterprise address. 
-- Note that the transformation  Address <-> Pkh is not symmetrical for all addresses
-- It's symmetrical for Enterprise addresses (because enterprise addresses have no stake Key in it)
pkhToMaybeAddr:: NetworkId -> PubKeyHash -> Maybe (AddressInEra  BabbageEra)
pkhToMaybeAddr network (PubKeyHash pkh) =do
    key <- vKey
    Just $ makeShelleyAddressInEra  network (PaymentCredentialByKey key)  NoStakeAddress
  where
    paymentCredential _vkey=PaymentCredentialByKey _vkey
    vKey= deserialiseFromRawBytes (AsHash AsPaymentKey) $fromBuiltin pkh


-- convert address to corresponding PubKeyHash. 
-- Note that the transformation  Address <-> Pkh is not symmetrical for all addresses
-- It's symmetrical for Enterprise addresses (because enterprise addresses have no stake Key in it)
addrToMaybePkh :: Cardano.Api.Shelley.Address ShelleyAddr -> Maybe PubKeyHash
addrToMaybePkh  = shelleyPayAddrToPlutusPubKHash


-- convert (AddressInEra) data type  to corresponding PubKeyHash. 
-- Note that the transformation  Address <-> Pkh is not symmetrical for all addresses
-- It's symmetrical for Enterprise addresses (because enterprise addresses have no stake Key in it)
addrInEraToPkh :: MonadFail m =>AddressInEra e -> m PubKeyHash
addrInEraToPkh a = case a of { AddressInEra atie ad -> case ad of
                                      ByronAddress ad' -> fail "Byron address is not supported"
                                      a@(ShelleyAddress net cre sr) -> case addrToMaybePkh a of
                                        Nothing -> fail "Expected PublicKey address got Script Address"
                                        Just pkh -> pure pkh }


-- convert the address to Enterprise Address. 
-- Enterprise address is an address having no stakeKey
unstakeAddr :: AddressInEra BabbageEra -> AddressInEra BabbageEra
unstakeAddr a = case a of { AddressInEra atie ad -> case ad of
                                      ByronAddress ad' ->a
                                      ShelleyAddress net cre sr ->  shelleyAddressInEra $ ShelleyAddress net cre StakeRefNull }


-- Create Plutus library AssetClass structure from Cardano.Api's AssetId
toPlutusAssetClass :: AssetId -> AssetClass
toPlutusAssetClass (AssetId (PolicyId hash) (AssetName name)) = AssetClass (CurrencySymbol $ toBuiltin $ serialiseToRawBytes hash , TokenName $ toBuiltin name)
toPlutusAssetClass AdaAssetId  =AssetClass (CurrencySymbol $ fromString "", TokenName $ fromString "")



-- Convert (ToData) (i.e. Plutus data) to Cardano.Api's ScriptData structure 
dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData =  fromPlutusData $ toData sData

toPlutusScriptHash = Alonzo.transScriptHash 

toPlutusCredential :: Credential keyrole crypto -> Plutus.Credential
toPlutusCredential  = Alonzo.transCred


addressToPlutusCredential :: Cardano.Api.Shelley.Address ShelleyAddr -> Plutus.Credential
addressToPlutusCredential (ShelleyAddress net cre sr) = toPlutusCredential cre

-- shelleyPayAddrToPlutusPubKHash :: Address ShelleyAddr -> Maybe Plutus.PubKeyHash
-- shelleyPayAddrToPlutusPubKHash (ShelleyAddress _ payCred _) =
--   case payCred of
--     Shelley.ScriptHashObj _ -> Nothing
--     Shelley.KeyHashObj kHash -> Just $ Alonzo.transKeyHash kHash

-- toPlutusAddress :: AddressInEra BabbageEra ->  Plutus.Address

toPlutusAddress :: Address ShelleyAddr -> Plutus.Address
toPlutusAddress (ShelleyAddress net cre sr) =Plutus.Address  (toPlutusCredential cre) (Alonzo.transStakeReference  sr)



addrInEraToPlutusAddress :: AddressInEra era -> Plutus.Address
addrInEraToPlutusAddress addr = case addr of { AddressInEra atie ad -> case ad of
                                        ByronAddress ad' -> error "addrInEraToPlutusAddress(): got byron address"
                                        ShelleyAddress net cre sr ->  Plutus.Address (toPlutusCredential cre) (Alonzo.transStakeReference  sr)   }

fromPlutusAddress :: NetworkId -> Plutus.Address -> Maybe (Address ShelleyAddr)
fromPlutusAddress network (Plutus.Address cre m_sc) = makeShelleyAddress network <$> paymentCre <*> stakingCre
  where
  paymentCre = case cre of
    Plutus.PubKeyCredential (PubKeyHash pkh) ->deserialiseFromRawBytes (AsHash AsPaymentKey) (fromBuiltin  pkh) <&> PaymentCredentialByKey
    Plutus.ScriptCredential (Plutus.ValidatorHash vh) -> deserialiseFromRawBytes AsScriptHash  (fromBuiltin  vh) <&> PaymentCredentialByScript
  stakingCre =  case m_sc of
    Nothing -> Just NoStakeAddress
    Just sc -> case sc of
      Plutus.StakingHash cre' -> case cre' of
        Plutus.PubKeyCredential (PubKeyHash pkh) -> deserialiseFromRawBytes (AsHash AsStakeKey ) (fromBuiltin  pkh) <&> StakeCredentialByKey  <&> StakeAddressByValue
        Plutus.ScriptCredential (Plutus.ValidatorHash vh) -> deserialiseFromRawBytes AsScriptHash (fromBuiltin  vh) <&> StakeCredentialByScript   <&> StakeAddressByValue
      Plutus.StakingPtr n i j -> Just $ fromShelleyStakeReference $ StakeRefPtr (Ptr (SlotNo $ fromInteger n) (Ledger.TxIx $ fromInteger i) (Ledger.CertIx $ fromInteger j))

--  then KeyHashObj (KeyHash addrHash)
--           else ScriptHashObj (ScriptHash addrHash)
--       addrHash :: Hash (CC.ADDRHASH crypto) a
--       addrHash =
--         hashFromPackedBytes $
--           PackedBytes28 a b c (fromIntegral (d `shiftR` 32))




-- transStakeReference (StakeRefPtr (Ptr (SlotNo slot) txIx certIx)) =
--   let !txIxInteger = toInteger (txIxToInt txIx)
--       !certIxInteger = toInteger (certIxToInt certIx)
--    in Just (PV1.StakingPtr (fromIntegral slot) txIxInteger certIxInteger)
-- transStakeReference StakeRefNull = Nothing

  --   data Credential
  -- = PubKeyCredential PubKeyHash -- ^ The transaction that spends this output must be signed by the private key
  -- | ScriptCredential ValidatorHash -- 