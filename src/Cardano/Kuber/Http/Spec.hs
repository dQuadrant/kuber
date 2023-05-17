{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Kuber.Http.Spec(
        KuberServerApi
      , QueryApi
      , KuberApi
      , UtilityApi
      , kuberApiServerProxy
      , CBORBinary
      , CBORText
      , AnyTextType

) where
import Servant.API
import Cardano.Api.Shelley
import Cardano.Kuber.Data.Models
import Data.Text
import Cardano.Kuber.Core.TxBuilder (TxBuilder)
import Data.Data (Proxy (Proxy))
import Cardano.Kuber.Http.MediaType
type KuberServerApi =
              "api" :>"v3"  :>    QueryApi
        :<|>  "api" :> "v1"  :> (
                        KuberApi
                  :<|>  UtilityApi
    )

type QueryApi =
            "protocol-params" :>  Get '[JSON] ProtocolParameters
      :<|>  "chain-point" :>  Get '[JSON] ChainPointModal
      :<|>  "utxo" :> QueryParams "address" Text :> QueryParams "txin" Text :>  Get '[JSON] UtxoModal
      :<|>  "system-start" :>  Get '[JSON] SystemStartModal
      :<|>  "genesis-params"  :> Get '[JSON] GenesisParamModal


type KuberApi =
            "tx"  :>  QueryParam "submit" Bool :> ReqBody '[JSON] TxBuilder :> Post '[JSON] TxModal
      :<|> "tx"   :>  "submit" :> ReqBody '[JSON] SubmitTxModal :>   Post '[JSON] TxModal
      :<|> "time" :>  Get '[JSON] TranslationResponse
      :<|> "time" :>  "toslot"  :> ReqBody '[JSON] TimeTranslationReq :> Post '[JSON] TranslationResponse
      :<|> "time" :>  "fromSlot" :> ReqBody '[JSON] SlotTranslationReq :> Post '[JSON] TranslationResponse


type UtilityApi =
--  "tx" :> "fee" :> QueryParams "shelleyWitCount" :> QueryParams "ByronWitCount" :> ReqBody '[ CBORBinary,CBORText,JSON ] TxModal :> Post '[JSON] Lovelace
            "tx" :> "fee" :> ReqBody '[ CBORBinary,CBORText,JSON ] TxModal :> Post '[JSON] Lovelace
       :<|> "tx" :> "exUnits" :> ReqBody '[CBORBinary, CBORText,CBORText] TxModal :> Post '[JSON] ExUnitsResponseModal



kuberApiServerProxy :: Proxy KuberServerApi
kuberApiServerProxy = Proxy