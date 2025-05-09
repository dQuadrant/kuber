# Sequence Diagrams

Interaction with Hydra

#### Initialize Head

Initialization of a Hydra head is the process of setting up the offchain state channel for a group of parties. It begins with an initiator posting an initial transaction on the mainchain, which defines the head members and parameters, and forges unique participation tokens for each member.

<img alt="Hydra head initialization sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/init.jpg" />

#### Commit UTxOs

Each member locks their chosen UTxOs onchain by posting a commit transaction, utilizing their participation token. Finally, a collectCom transaction is posted to aggregate all committed UTxOs into the initial head state and transition the mainchain state machine to the "open" state, enabling offchain processing to commence.

<img alt="Commit UTxOs to hydra head sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/commit.jpg" />

#### DeCommit UTxOs

Decommitting a UTxO in Hydra means moving it out of the offchain head protocol and back onto the mainchain. This effectively makes the funds or state represented by that UTxO available for standard layer-one operations again. Decommitment can occur when the head is closed, settling the final set of UTxOs back onchain, or incrementally while the head remains open, allowing specific UTxOs to be removed without closing the entire head.

<img alt="DeCommit UTxOs from hydra head sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/decommit.jpg" />

#### Close Head

Closing a Hydra head means terminating the offchain state channel and settling the final agreed-upon state back onto the mainchain. This process is typically initiated by a head member posting a close transaction on the mainchain, which transitions the mainchain state machine from the open state to the closed state. This transaction includes a certificate representing that party's view of the latest confirmed offchain state.

<img alt="Close hydra head sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/close.jpg" />

#### Query UTxOs

Hydra head maintains a record of available UTxOs that can be spent.

<img alt="Query hydra UTxOs sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/utxo.jpg" />

#### Query Protocol Parameters

Hydra can use its own custom protocol parameters, and it's common to set the transaction fees to zero in these settings to optimize token usage and reduce costs.

<img alt="Query hydra UTxOs sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/protocol-parameters.jpg" />

#### Abort Head

Aborting a Hydra head is a process initiated on the mainchain during the setup phase if the head initialization fails. This failure typically occurs when not all head members successfully post their commit transactions to lock their UTxOs into the head. A party can then post an abort transaction on the mainchain. This transaction transitions the mainchain state machine directly from the initial state to the final state, bypassing the open and closed states. The abort transaction ensures that any UTxOs that were successfully committed are returned to the mainchain and that the head's unique participation tokens are burned, effectively cancelling the head creation and cleaning up the onchain state.

<img alt="Abort hydra head sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/abort.jpg" />

#### Fanout Head

Finally, after the contestation period, a fanout transaction transitions the state machine to the final state, placing the definitive set of UTxOs from the head back onto the mainchain, effectively replacing the initial set that was committed.

<img alt="Fanout hydra head sequence diagram" src="https://raw.githubusercontent.com/dQuadrant/kuber/88c3fe0b1e721e5e4fb8e4bcd9e50f10b570f628/hydra/sequence-diagrams/fanout.jpg" />
