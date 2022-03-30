module Cardano.Contrib.Easy.Spec
where
  
data InputUtxoModal = AddressInputUtxo {
      ireqUtxo :: UtxoIdModal,
    } |
    ScriptInputUtxo {
      ireqUtxo :: UtxoIdModal,
      ireqmDatum ::ScriptData ,
      ireqMExUnits :: Maybe ( ExecutionUnits),
      ireqScript :: Script
    }

data InputAddressModal =InputAddressMOdal{
  iAddress :: Maybe (AddressInEra AlonzoEra),
  iSpendEverything :: Bool,
}

data MintRequestModal = MintRequestModal{
    mintReqScript :: Script 
}

data PaymentUtxoModel = PaymentUtxoModel {
  paymentValue :: Cardano.Api.Shelley.Value,
  receiverAddress:: AddressInEra AlonzoEra,
  deductFees :: Bool, -- pay this address paymentValue -txFee.
  addChange :: Bool
}

data PaymentReqModel = PaymentReqModel {
  preqMint ::  Cardano.Api.Shelley.Value,
  preqMintingScript :: [Script],
  preqReceivers::[PaymentUtxoModel],
  preqInputs:: [InputUtxoModal],
  preqCollateral:: [UtxoIdModal]
  preqAvailableUtxos :: [UtxoIdmodal],
  preqPayerAddress:: [InputAddressModal],
  preqChangeAddress :: Maybe(AddressInEra AlonzoEra),
  preqValidFrom :: Maybe  (Integer),
  preqValidUntil :: Maybe (Integer)
}