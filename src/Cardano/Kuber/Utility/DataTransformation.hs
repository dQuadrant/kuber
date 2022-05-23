{-# LANGUAGE GADTs #-}
module Cardano.Kuber.Utility.DataTransformation 
where
import Cardano.Api
import qualified Cardano.Api.Shelley as Shelley
import Cardano.Api.Byron (Address(ByronAddress))
import Cardano.Api.Shelley (Address(ShelleyAddress), fromPlutusData, fromShelleyPaymentCredential)
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash), ToData, CurrencySymbol (CurrencySymbol), TokenName (TokenName), toBuiltin, toData, fromBuiltin)
import Data.ByteString
import qualified Cardano.Binary as Cborg
import Cardano.Ledger.Shelley.API (Credential(KeyHashObj, ScriptHashObj), StakeReference (StakeRefNull))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import Data.String (fromString)


skeyToAddr:: SigningKey PaymentKey -> NetworkId -> Shelley.Address ShelleyAddr
skeyToAddr skey network =
  makeShelleyAddress  network  credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey

skeyToAddrInEra ::  SigningKey PaymentKey -> NetworkId -> AddressInEra AlonzoEra
skeyToAddrInEra skey network=makeShelleyAddressInEra network   credential NoStakeAddress
  where
    credential=PaymentCredentialByKey  $ verificationKeyHash   $ getVerificationKey  skey

addressInEraToAddressAny :: AddressInEra era -> AddressAny
addressInEraToAddressAny addr = case addr of { AddressInEra atie ad -> toAddressAny ad }


sKeyToPkh:: SigningKey PaymentKey -> PubKeyHash
sKeyToPkh skey= PubKeyHash (toBuiltin  $  serialiseToRawBytes  vkh)
  where
    vkh=verificationKeyHash   $ getVerificationKey  skey

addressInEraToPaymentKeyHash :: AddressInEra AlonzoEra -> Maybe (Hash PaymentKey)
addressInEraToPaymentKeyHash a = case a of { AddressInEra atie ad -> case ad of
                                               ByronAddress ad' -> Nothing
                                               ShelleyAddress net cre sr -> case fromShelleyPaymentCredential cre of
                                                 PaymentCredentialByKey ha -> Just ha
                                                 PaymentCredentialByScript sh -> Nothing
                                    }

pkhToMaybeAddr:: NetworkId -> PubKeyHash -> Maybe (AddressInEra  AlonzoEra)
pkhToMaybeAddr network (PubKeyHash pkh) =do
    key <- vKey
    Just $ makeShelleyAddressInEra  network (PaymentCredentialByKey key)  NoStakeAddress
  where
    paymentCredential _vkey=PaymentCredentialByKey _vkey
    vKey= deserialiseFromRawBytes (AsHash AsPaymentKey) $fromBuiltin pkh

addrToMaybePkh :: Cardano.Api.Shelley.Address ShelleyAddr -> Maybe PubKeyHash
addrToMaybePkh (ShelleyAddress net cre sr) = do
  PubKeyHash . toBuiltin <$> hash
  where
    hash= case cre of
      ScriptHashObj _ ->Nothing
      KeyHashObj kh -> pure ( Cborg.serialize' kh)


addrInEraToPkh :: MonadFail m =>AddressInEra AlonzoEra -> m PubKeyHash
addrInEraToPkh a = case a of { AddressInEra atie ad -> case ad of
                                      ByronAddress ad' -> fail "Byron address is not supported"
                                      a@(ShelleyAddress net cre sr) -> case addrToMaybePkh a of
                                        Nothing -> fail "Expected PublicKey address got Script Address"
                                        Just pkh -> pure pkh }


unstakeAddr :: AddressInEra AlonzoEra -> AddressInEra AlonzoEra
unstakeAddr a = case a of { AddressInEra atie ad -> case ad of
                                      ByronAddress ad' ->a
                                      ShelleyAddress net cre sr ->  shelleyAddressInEra $ ShelleyAddress net cre StakeRefNull }



toPlutusAssetClass :: AssetId -> AssetClass
toPlutusAssetClass (AssetId (PolicyId hash) (AssetName name)) = AssetClass (CurrencySymbol $ toBuiltin $ serialiseToRawBytes hash , TokenName $ toBuiltin name)
toPlutusAssetClass AdaAssetId  =AssetClass (CurrencySymbol $ fromString "", TokenName $ fromString "")



dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData =  fromPlutusData $ toData sData