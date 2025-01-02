# Hydra - Kuber 🤝🏼

## High-Level Spec

The objective of this proposal is to integrate Hydra into the **Kuber** haskell library. This integration will be implemented via a new class, which will replicate and adapt the core functionalities already present in the Kuber library, with specific modifications to support Hydra. And of course all of these functionalities will be available through APIs.

For the Kuber playground, we will introduce a UI to visualize connected Hydra peers and monitor the current Hydra state. Similar to the existing json transaction builder, users will be able to build, sign, and submit Hydra transactions directly from the UI using our JSON format.

In the context of the Kuber-client npm package, we will implement Hydra functionalities in JavaScript. This will include features such as funding Hydra addresses from a faucet and starting a Hydra node, provided the user has the necessary dependencies installed on their system.

## Detailed Spec

#### **Prerequisites**

1. **Hydra Scripts TxId**:  
   The transaction ID for the Hydra scripts, available for both testnets and mainnet.  
   [Hydra releases](https://github.com/cardano-scaling/hydra/releases)
2. **Keys**:
   - Node keys
   - Fund keys
   - Hydra keys
3. **Running Hydra Node**:  
   A Hydra node with all participants must be active.
4. **Running Cardano Node**:  
   A Cardano node socket must be operational.

#### **Functionality for Kuber Library**

Upon project completion, the following features will be available:

1. **Head Initialization**:

   - **Purpose**: Submit an on-chain transaction to establish a Hydra Head connection between participants.

2. **Commit UTxOs**:

   - **Purpose**: Enable users to commit or decommit their UTxOs to the Hydra Head.

3. **Abort Head**:

   - **Purpose**: Allow participants to abort the Head if UTxOs are not committed within the required timeframe.

4. **Query UTxOs**:

   - **Purpose**: Retrieve the UTxOs committed to the Hydra Head.
   - **Filtering**: By TxId, address, or value.

5. **Check Commit Status**:

   - **Purpose**: Check the status of committed UTxOs within the Hydra Head.

6. **Build Raw Transaction**:

   - **Purpose**: Create raw transactions for minting, burning, redeeming, funding a smart contract, or redeeming from a smart contract within the Head.
   - **Fee Balancing**: Automatically calculate and balance transaction fees based on Hydra’s protocol parameters.

7. **Sign and Submit Transactions**:

   - **Options**:
     - Return the signed transaction.
     - Directly sign and submit the transaction.
   - **Purpose**: Users must provide their Hydra signing key to sign and submit transactions.

8. **Close Transaction**:

   - **Purpose**: Submit an on-chain transaction to close the Hydra Head, including a snapshot of its current state.
   - **Known Issue**: [hydra/issues/#1039](https://github.com/cardano-scaling/hydra/issues/1039)

9. **Contestation Period**:

   - **Purpose**: Enable participants to contest during the contestation period.

10. **Fanout Transactions**:
    - **Purpose**: Distribute the final state of the Hydra Head to the main chain.

#### **UI Functionalities for Kuber Playground**

1. **Hydra Peer Visualization**

   - Display a list of connected Hydra peers in real-time, showing their IP addresses, connection status, and verification keys.
   - Show peer statuses (e.g., connected, disconnected) with the ability to refresh and view the most current peer data.

2. **Monitor Hydra State**

   - Visualize the current state of the Hydra Head, including active participants, ongoing transactions, and UTxO list.
   - Display the status of the Hydra Head (e.g., idle, initialized, active, closed, contested)
   - Provide real-time updates and allow users to view logs or history of actions related to the Hydra Head.
   - Allow users to abort the Hydra Head if participants fail to commit UTxOs within the allotted time, with an easy-to-use interface for initiating the action.

3. **Transaction Builder (JSON Format)**

   - Enable users to create and customize Hydra transactions by specifying details in JSON like inputs, outputs, mint etc.
   - Allow users to build transaction for minting, burning, funding, or redeeming from smart contracts within hydra.

4. **Sign and Submit Transaction (with wallet extension)**

   - Automatically generate the transaction CBOR from corresponding JSON format.
   - Sign the transaction using the user’s Hydra signing key.
   - Offer options for users to either receive the signed transaction for manual submission or automatically submit it to the Hydra node.
   - Show a confirmation of successful submission or an error message if there’s an issue with the transaction.

5. **Transaction History and Logs**

   - Offer a searchable history of Hydra transactions, displaying transaction IDs, timestamps, and status.
   - Allow users to view detailed transaction information such as inputs, outputs, mints etc.

6. **Hydra Head Control Actions**
   - Allow users to close the Hydra Head by submitting an on-chain transaction.
   - Enable the option for users to contest the close action during the contestation period with an intuitive interface.
   - Implement the “Fanout” feature to distribute the final state of the Hydra Head to the main chain after the contestation period.

#### **Functionalities for Kuber-Client npm package**

- > The features will be the same as kuber haskell library, but will be available in a npm package

#### **Expected Outcome**

Integrating Hydra into the Kuber library will enable users to leverage Hydra’s off-chain scalability benefits without requiring deep technical knowledge of Hydra. The solution will offer intuitive interfaces for transactions, ensuring a consistent experience with the current Kuber functionality.

#### Potential Issues

- Transactions within the Hydra Head must be properly balanced. Ideally, the fee should be set to 0 to retain all value within the Hydra Head.
- A test transaction was conducted with a non-zero default fee but without specifying a fee output. While the transaction was processed inside Hydra, an error occurred while closing the Head, indicating the transaction was unbalanced, which could potentially lock funds.

## Developement Timeline

- kuber haskell library integration and testing: 3 months
- kuber-client npm package integration and testing: 1 month
- kuber playgroung integration and testing: 2 months

## Dileverables

#### Kuber API

- **Deliverables**
  - Ability to connect to Hydra node and query network parameters/state
  - API functionality for Hydra transaction building
  - Extensive test coverage
- **Acceptance Criteria**
  - Successful implementation of the API and Haskell library
  - All tests pass successfully

#### Kuber playground

- **Deliverables**
  - Real-time display of a connected Hydra peer and its status.
  - Visualization of Hydra Head status, participants, and UTxO details.
  - Interface for creating Hydra transactions using JSON.
  - Option to sign and submit transactions directly from the UI using browser wallet extension (if supported by extension)
  - Options to close the Hydra Head, contest the close, and distribute the final state.
- **Acceptance Criteria**
  - Hydra peer and connection status correctly displayed and updated in real time.
  - Hydra Head state and transaction history are accurately shown.
  - Users can create, sign, and submit transactions easily.
  - The UI supports closing, contesting, and finalizing the Hydra Head state.

#### **Kuber Client**

- **Deliverables**
  - A functional NPM package supporting all the hydra features in kuber server library
  - Comprehensive test coverage to ensure reliability and robustness.
  - Compatibility with DApps, enabling seamless integration with decentralized applications
  - Ability to connect to wallet and sign transactions.
- **Acceptance Criteria**
  - Successful implementation of all Hydra-related features within the NPM package.
  - All tests pass.
  - Verified compatibility with DApps, including successful execution of Hydra operations in a DApp environment.
