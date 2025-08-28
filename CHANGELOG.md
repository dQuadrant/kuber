## v4.0.0 : Revamp Docs, Hydra features
This release includes all changes from `v4.0.0-rc1` and the following updates:
### Fixes
- Refactors and fixes decommit API endpints 

### Changes 
- Revamp docs section
- Update docs for decommit
- Add yarn doc:validate command
- Add common kuber functions
- Add tx-builder reference to buildTx reference
- Fix kube-hydra link in Readme
- Improve getting started section
- Fix kuber-client link in docs
- Fix navigation in docs
- Update Readme, Refactor docs


##### Libraries
- cardano-api:10.1
- [kuber-client:v3.3.5](https://www.npmjs.com/package/kuber-client/v/3.3.5)


##### Tested with
- hydra-node:0.22.4-d505ff119c4bc62f008b6fcff3416cffc3d6df5b


## v4.0.0-rc1 : Hydra Integration
This release introduces **kuber-hydra**, a dedicated module that adds full Hydra head
support to Kuber. The new module exposes a set of **command** and **query** APIs for
**committing**, **decommitting**, **closing**, **fan‑out**, **contesting**, and **querying** the state of a
Hydra head, along with a **transaction‑builder** helper and APIs.

**Note :** `kuber-hydra` executable additionally exposes all layer1 endpoints making it versatile for both hydra and l1 interaction.

##### DependsOn
- cardano-api:10.1

##### Tested with
- hydra-node:0.22.0-65b4c1201e7533d4269d42e2ca1c8b53f999454f

### Changes: kuber-hydra
- Added new module `kuber-hydra` with:
  - **Command endpoints** under `/api/hydra`:
    - **POST** /hydra/init
    - **POST** /hydra/abort
    - **POST** /hydra/commit
    - **POST** /hydra/decommit
    - **POST** /hydra/close
    - **POST** /hydra/contest
    - **POST** /hydra/fanout
    - **POST** /hydra/tx
    - **POST** /hydra/submit
  - **Query endpoints** under `/api/hydra/query`:
    - **GET** /hydra/query/utxo
    - **GET** /hydra/query/head
    - **GET** /hydra/query/protocol-parameters
    - **GET** /hydra/query/state
    - **GET** /hydra/query/commits
- Implemented a Hydra transaction‑builder that:
  - Selects inputs/outputs
  - Handles optional signing keys
  - Filters UTxOs by txin and address
- Updated Hydra‑specific documentation and added sequence diagrams illustrating
  typical Hydra head interactions.

## v3.1.2 : Upgrade cardano-api to 10.x

### Changes
  - Bump cardano-api version to 10.1


## v3.1.1 : Collateral Selection Fix

### Fixes
  - Collateral selection logic now prefers 5Ada inputs
  - Multiple collateral inputs if collateral is not sufficient with 1 utxo.

## v3.1.0 : Stable Conway Era Support
This stable release supports adds full support for new conway governance features.

##### DependsOn
- cardano-api:9.2.0.0

### Changes: kuber-server
- new endpoint `/api/v3/health` now returns nodes current tip and secondsSince last block was minted. If lastBlock was seen before 300 secs, status code will be `503`

### Changes: lib:kuber
- when not set, kuber will automatically fill in following field by querying ledger state
  - `proposal.deposit`
  - `proposal.previousGovActionId`
  - `proposal.executionUnits`
  - `certificates.deposit`

### Fixes
- Fix catchError function on Unhandled error
- Add exception handler on txSubmission
- Use FrameworkError explicitly to improve error handling
- Make Kontract instance of MonadError and Alternative
- Show proper error message for plutus error
- add ex-unit test
- Fix fromJson of proposal field
- fix utxoModal parsing problem

#### Known Issues:
-  **(lib:kuber)**  : `Kontract` will omit following fields when using `RemoteKuberConnection` :  **$.proposal[s]**, **$.votes[s]** or **$.certificate[s]**

#### Litmiations:
 - voting with script key is not supported


## v3.0.0-rc2 : Tx build bug fix , Add tests
- Fix Bug : Server got irresponsive after requesting `/api/v1/tx` with plutus included transaction
- Add tests to validate that the kuber apis are working via LocalNode Socket connection.

#### Known Issues:
- `Kontract` is not working via `RemoteKuberConnection`
- If node is at Babbage era when kuber-server starts, after node reaches Conway era, restart is required  to enable Conway features.
- `$.validtyStart` is broken and is ignored.

## v3.0.0-rc1 : Full Refactor and ConwayEra Support

### Changes on kuber-server
- Add new chain-query api-endpoints.
    * **GET** /api/v3/protocol-params
    * **GET** /api/v3/chain-point
    * **GET** /api/v3/utxo
    * **GET** /api/v3/system-start
    * **GET** /api/v3/genesis-params
- Update default healthcheck url in docker image. (`/api/v3/chain-point` is used for healthcheck )
- Server starts by querying NodeEra and enables fields based on `BabbageEra` or `ConwayEra`
- TxBuilder json now supports following new fields.
    * **$.proposal[s]** Conway Era Governance proposals.
    * **$.votes[s]** Conway Era votes.
    * **$.certificate[s]** Registration and dregistration certs only for Conway era. (Not available when node is running in babbage era).


#### Known Issues:
- If node is at Babbage era when kuber-server starts, after node reaches Conway era, restart is required  to enable Conway features.
- `$.validtyStart` is broken and is ignored.


### Changes on Kontract-example [WIP]
- Introduced new package to showcase usage of `Kontract`


### Changes on lib:kuber
- Update code comments for better haddock docs generation
- Implement both `ToJson` and `FromJson` for all the entities used in api
- Reorganize code and  introduction of  `Kontact` for offchain code.
- Support For ConwayEra **[BREAKING change ]**
- `TxBuilder`  is now parametrized on `era` suppporting `Babbage` and `Conway` eras, with `IsTxBuilderEra` constraint.
- Kuber Offchain code required Local Node Socket. **WIP** support running kuber code by connecting to kuber server
- Remove `ChainConnectInfo`and related classes.  Instead, use `LocalNodeConnectInfo CardanoMode` directly.
