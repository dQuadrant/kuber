{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE  OverloadedStrings #-}
module Api.Swagger where
import Servant.Swagger
import Data.Swagger 
import qualified Data.Swagger as Swagger
import Servant.Swagger.UI
import Servant
import Websocket.Aeson
import Api.Spec (API, server, CommitUTxOs)
import Control.Lens ((.~), (?~))
import Control.Lens.Lens
import Cardano.Kuber.Api (TxBuilder_, IsTxBuilderEra)
import qualified Data.Aeson as A
import Cardano.Api (TxIn)


instance (IsTxBuilderEra era) => (Swagger.ToSchema (TxBuilder_ era) ) where
  declareNamedSchema _ = do
      let schema = mempty
            & type_ ?~ Swagger.SwaggerObject
      return $ NamedSchema (Just "StringMap") schema

instance ToSchema A.Value where
  declareNamedSchema _ = return $
    NamedSchema (Just "AnyValue") mempty

instance ToSchema TxIn where
  declareNamedSchema _ = return $
    NamedSchema (Just "TxIn") $ mempty
      & type_ ?~ SwaggerString
      & Swagger.description ?~ "Transaction input represented as a string"
      
instance ToSchema CommitUTxOs

hydraSwagger :: Swagger
hydraSwagger = toSwagger (Proxy :: Proxy API)
  & Swagger.info . Swagger.title       .~ "Hydra API"
  & Swagger.info . Swagger.version     .~ "1.0"
  & Swagger.info . Swagger.description ?~ "API for interacting with Hydra."

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type SwaggerUI = SwaggerSchemaUI "docs" "swagger.json"

type FullAPI = API :<|> SwaggerAPI :<|> SwaggerUI


serverWithSwagger :: AppConfig -> Server FullAPI
serverWithSwagger appConfig =
  server appConfig
    :<|> pure hydraSwagger
    :<|> swaggerSchemaUIServer hydraSwagger
