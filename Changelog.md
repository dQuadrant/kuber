
## v3.0.0-rc1
- Update code comments for better haddock docs generation
- Implement both `ToJson` and `FromJson` for all the entities used in api
- Reorganize code and  introduction of  Kontact for offchain code.
- Add new chain-query api-endpoints.
    * **GET** /api/v3/protocol-params
    * **GET** /api/v3/chain-point
    * **GET** /api/v3/utxo
    * **GET** /api/v3/system-start
    * **GET** /api/v3/genesis-params
- Update default healthcheck url (`/api/v3/chain-point` is used for healthcheck )
