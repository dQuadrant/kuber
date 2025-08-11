---
sidebar_position: 2
title: Kuber API Reference
---

import ApiEndpoint from '@site/src/components/ApiEndpoint';

# Kuber API Reference

This section provides detailed documentation for the Kuber backend API endpoints.

## Query API

<ApiEndpoint
  method="GET"
  path="/api/v3/protocol-params"
  title="Retrieve Protocol Parameters"
  description="Retrieves the current protocol parameters for the Cardano network."
  responses={[
    { code: "200 OK", description: "Returns the protocol parameters." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/api/v3/chain-point"
  title="Retrieve Chain Point"
  description="Retrieves the current chain point (block number and hash) of the Cardano network."
  responses={[
    { code: "200 OK", description: "Returns the current chain point." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/api/v3/utxo"
  title="Query UTxOs"
  description="Queries UTxOs based on provided addresses and/or transaction inputs."
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
  path="/api/v3/system-start"
  title="Retrieve System Start"
  description="Retrieves the system start time of the Cardano network."
  responses={[
    { code: "200 OK", description: "Returns the system start time." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/api/v3/current-era"
  title="Retrieve Current Era"
  description="Retrieves the current era of the Cardano network."
  responses={[
    { code: "200 OK", description: "Returns the current Cardano era." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/api/v3/genesis-params"
  title="Retrieve Genesis Parameters"
  description="Retrieves the genesis parameters of the Cardano network."
  responses={[
    { code: "200 OK", description: "Returns the genesis parameters." },
  ]}
/>


## Kuber API

<ApiEndpoint
  method="POST"
  path="/api/v1/tx"
  title="Build Transaction"
  description="Builds a transaction based on the provided TxBuilder."
  queryParams={[
    { name: "submit", type: "Boolean", description: "If true, the transaction will be submitted to the chain.", optional: true },
  ]}
  requestBody={{
    type: "TxBuilder",
    description: "Details for building the transaction.",
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
  path="/api/v1/tx/submit"
  title="Submit Transaction"
  description="Submits a transaction to the Cardano network."
  requestBody={{
    type: "SubmitTxModal",
    description: "Signed transaction details.",
    fields: [
      { name: "signedTx", type: "string", description: "The signed transaction." },
      { name: "witnesses", type: "Array", description: "Transaction witnesses." },
      // Add more TxModal fields as needed based on its actual structure
    ],
  }}
  responses={[
    { code: "200 OK", description: "Success message." },
  ]}
/>

<ApiEndpoint
  method="GET"
  path="/api/v1/time"
  title="Retrieve Current Time"
  description="Retrieves the current time from the Cardano network."
  responses={[
    { code: "200 OK", description: "Returns the current time." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/api/v1/time/toslot"
  title="Translate POSIX Time to Slot"
  description="Translates a given POSIX time to a Cardano slot number."
  requestBody={{
    type: "TimeTranslationReq",
    description: "Request body for POSIX time to slot translation.",
    fields: [
      { name: "posixTime", type: "Integer", description: "The POSIX time to translate." },
    ],
  }}
  responses={[
    { code: "200 OK", description: "Returns the corresponding slot number." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/api/v1/time/fromSlot"
  title="Translate Slot to POSIX Time"
  description="Translates a given Cardano slot number to POSIX time."
  requestBody={{
    type: "SlotTranslationReq",
    description: "Request body for slot to POSIX time translation.",
    fields: [
      { name: "slot", type: "Integer", description: "The slot number to translate." },
    ],
  }}
  responses={[
    { code: "200 OK", description: "Returns the corresponding POSIX time." },
  ]}
/>

## Utility API

<ApiEndpoint
  method="POST"
  path="/api/v1/tx/fee"
  title="Calculate Minimum Fee"
  description="Calculates the minimum fee required for a given transaction."
  requestBody={{
    type: "TxModal",
    description: "The transaction for which to calculate the fee.",
    fields: [
      // Add TxModal fields as needed
    ],
  }}
  responses={[
    { code: "200 OK", description: "Returns the minimum fee (Coin)." },
  ]}
/>

<ApiEndpoint
  method="POST"
  path="/api/v1/tx/exUnits"
  title="Calculate Execution Units"
  description="Calculates the execution units (ExUnits) for a given transaction."
  requestBody={{
    type: "TxModal",
    description: "The transaction for which to calculate execution units.",
    fields: [
      // Add TxModal fields as needed
    ],
  }}
  responses={[
    { code: "200 OK", description: "Returns the execution units (ExUnitsResponseModal)." },
  ]}
/>

## Health

<ApiEndpoint
  method="GET"
  path="/api/v3/health"
  title="Cardano Health Check"
  description="Performs a health check to verify the connection and sync-status of the Cardano node."
  responses={[
    { code: "200 OK", description: "Cardano node is healthy." },
    { code: "500 Internal Server Error", description: "Cardano node is not available or an error occurred." },
  ]}
/>