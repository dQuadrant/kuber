---
sidebar_position: 1
title: Kuber-Hydra API Reference
---

import ApiEndpoint from '@site/src/components/ApiEndpoint';

# Kuber-Hydra API Reference

Kuber-Hydra provides following endpoints in addition to the layer1 kuber endpoints.
When running hydra server, all of the endpoints in this doc and in kuber doc will be available

## Hydra Command API

<ApiEndpoint
  method="POST"
  path="/hydra/init"
  title="Initialize Hydra Head"
  description="Initializes a new Hydra Head."
  queryParams={[
    { name: "wait", type: "Boolean", description: "If true, the request will wait for the transaction to be confirmed.", optional: true },
  ]}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/abort"
  title="Abort Hydra Head"
  description="Aborts an existing Hydra Head."
  queryParams={[
    { name: "wait", type: "Boolean", description: "If true, the request will wait for the transaction to be confirmed.", optional: true },
  ]}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/commit"
  title="Commit UTxOs to Hydra Head"
  description="Commits UTxOs to the Hydra Head."
  queryParams={[
    { name: "submit", type: "Boolean", description: "If true, the transaction will be submitted to the chain.", optional: true },
  ]}
  requestBody={{
    type: "CommitUTxOs",
    description: "Payload containing UTxOs to commit and an optional signing key.",
    fields: [
      { name: "utxos", type: "Array of TxIn (string)", description: "List of transaction inputs to commit." },
      { name: "signKey", type: "A.Value", description: "The signing key for the transaction.", optional: true },
    ],
  }}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/hydra/decommit"
  title="Build Decommit Transaction"
  description="Builds a decommit transaction."
  queryParams={[
    { name: "txin", type: "Array of T.Text", description: "List of transaction inputs to decommit." },
  ]}
  responses={[
    { code: "200 OK", description: "Returns TxModal (transaction details)." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/decommit"
  title="Submit Decommit Transaction"
  description="Submits a decommit transaction."
  queryParams={[
    { name: "wait", type: "Boolean", description: "If true, the request will wait for the transaction to be confirmed.", optional: true },
  ]}
  requestBody={{
    type: "TxModal",
    description: "Signed transaction details.",
    fields: [
      { name: "cborHex", type: "string", description: "The CBOR-encoded transaction in hexadecimal format." },
      { name: "type", type: "string", description: "The type of transaction (e.g., 'Witnessed Tx ConwayEra')." },
      { name: "description", type: "string", description: "An optional description for the transaction." },
    ],
  }}
  responses={[
    { code: "200 OK", description: "Returns DecommitResult (decommit transaction details)." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/close"
  title="Close Hydra Head"
  description="Closes the Hydra Head."
  queryParams={[
    { name: "wait", type: "Boolean", description: "If true, the request will wait for the transaction to be confirmed.", optional: true },
  ]}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/contest"
  title="Contest Hydra Head"
  description="Contests the Hydra Head."
  queryParams={[
    { name: "wait", type: "Boolean", description: "If true, the request will wait for the transaction to be confirmed.", optional: true },
  ]}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/fanout"
  title="Fanout Hydra Head"
  description="Fans out the remaining UTxOs from the Hydra Head."
  queryParams={[
    { name: "wait", type: "Boolean", description: "If true, the request will wait for the transaction to be confirmed.", optional: true },
  ]}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/tx"
  title="Build Transaction"
  description="Builds a transaction."
  queryParams={[
    { name: "submit", type: "Boolean", description: "If true, the transaction will be submitted to the chain.", optional: true },
  ]}
  requestBody={{
    type: "TxBuilder",
    description: (
      <span>
        Details for building the transaction. See{" "}
        <a href="https://kuberide.com/kuber/docs/tx-builder-reference" target="_blank" rel="noreferrer">
          TxBuilder object reference
        </a>
        .
      </span>
    ),
    fields: [
      { name: "inputs", type: "Array", description: "List of transaction inputs." },
      { name: "outputs", type: "Array", description: "List of transaction outputs." },
      { name: "metadata", type: "Object", description: "Transaction metadata." },
      // Add more TxBuilder fields as needed based on its actual structure
    ],
  }}
  responses={[
    { code: "200 OK", description: "Returns TxModal (transaction details)." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/hydra/submit"
  title="Submit Transaction"
  description="Submits a transaction."
  queryParams={[
    { name: "wait", type: "Boolean", description: "If true, the request will wait for the transaction to be confirmed.", optional: true },
  ]}
  requestBody={{
    type: "TxModal",
    description: "Signed transaction details.",
    fields: [
      { name: "cborHex", type: "string", description: "The CBOR-encoded transaction in hexadecimal format." },
      { name: "type", type: "string", description: "The type of transaction (e.g., 'Witnessed Tx ConwayEra')." },
      { name: "description", type: "string", description: "An optional description for the transaction." },
    ],
  }}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

## Hydra Query API

<ApiEndpoint
  method="GET"
  path="/hydra/query/utxo"
  title="Query UTxOs"
  description="Queries UTxOs."
  queryParams={[
    { name: "address", type: "Array of T.Text", description: "Filter UTxOs by address.", optional: true },
    { name: "txin", type: "Array of T.Text", description: "Filter UTxOs by transaction input.", optional: true },
  ]}
  responses={[
    { code: "200 OK", description: "Returns a list of UTxOs." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/hydra/query/head"
  title="Retrieve Hydra Head State"
  description="Retrieves the current state of the Hydra Head."
  responses={[
    { code: "200 OK", description: "Returns the Hydra Head state." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/hydra/query/protocol-parameters"
  title="Retrieve Hydra Protocol Parameters"
  description="Retrieves the Hydra protocol parameters."
  responses={[
    { code: "200 OK", description: "Returns the protocol parameters." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/hydra/query/state"
  title="Retrieve Hydra State"
  description="Retrieves the current state of the Hydra."
  responses={[
    { code: "200 OK", description: "Returns an object containing the current HydraHeadState (e.g., { state: 'Open' })." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/hydra/query/commits"
  title="Retrieve Committed UTxOs"
  description="Retrieves the committed UTxOs in the Hydra Head."
  responses={[
    { code: "200 OK", description: "Returns a list of committed UTxOs." },
  ]}
/>
