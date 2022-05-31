{-# LANGUAGE GADTs #-}
module Cardano.Kuber.Utility.DataTransformation 
where
import Cardano.Api
import qualified Cardano.Api.Shelley as Shelley
import Cardano.Api.Byron (Address(ByronAddress))
import Cardano.Api.Shelley (Address(ShelleyAddress), fromPlutusData, fromShelleyPaymentCredential, shelleyPayAddrToPlutusPubKHash)
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash, getPubKeyHash), ToData, CurrencySymbol (CurrencySymbol), TokenName (TokenName), toBuiltin, toData, fromBuiltin)
import Data.ByteString
import qualified Cardano.Binary as Cborg
import Cardano.Ledger.Shelley.API (Credential(KeyHashObj, ScriptHashObj), StakeReference (StakeRefNull))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import Data.String (fromString)
import Cardano.Ledger.Alonzo.TxInfo (transKeyHash)


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