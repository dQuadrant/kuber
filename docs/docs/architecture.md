---
sidebar_position: 1
---

# Architecture

### Architecture Diagram

![Kuber-Hydra Architecture](/img/kuber-hydra.drawio.png)

## Services

### Kuber Playground

Kuber Playground is an intuitive platform designed for users to interact seamlessly with Hydra node. This playground provides a comprehensive interface for managing Hydra heads, simplifying complex operations, and ensuring a smooth user experience for developers and testers alike.

In this service, users can:
- Connect their browser wallet to the Hydra environment effortlessly.
- Query vital information such as snapshots, protocol parameters, committed UTxOs, and participant verification keys by simply providing the IP address and host port of the running Hydra node.
- Perform various Hydra head operations through the user-friendly interface, including initializing the Hydra head, committing and de-committing UTxOs, aborting the head, closing it, contesting, and executing a fanout.
- Write and submit transactions in a simple JSON format to the Hydra node with just a few clicks.
- Monitor logs and transaction states, check UTxO details, and track their balance in real-time.
- Validate that the Hydra head closure and fanout process will proceed without issues, ensuring reliability and accuracy.

The Kuber Playground empowers users by providing a streamlined, accessible way to interact with Hydra nodes, making it ideal for developers and enthusiasts looking to test or deploy solutions in a Hydra-enabled environment.

### Kuber Server (Backend)
- The backend establishes and maintains a WebSocket connection with the Hydra node, enabling seamless communication and real-time updates of the Hydra head state to the frontend.  
- It processes the Hydra head state, including participants' verification keys, commitments, head ID, UTxOs, and balances, and sends this information to the frontend for user visibility.  
- The backend validates user-created transactions against the current UTxO set and protocol parameters, ensuring that all transactions comply with the Hydra protocol rules before submission.  
- It receives signed transactions from the frontend and submits them to the Hydra node through the WebSocket connection, facilitating reliable execution.  
- The backend checks for potential issues during the closure or fan-out process and provides users with feedback on errors or successful completion.  
- It supports critical operations like initializing the Hydra head, committing/de-committing UTxOs, closing the head, and enabling the fan-out process.  
- The backend ensures secure processing of sensitive data, such as user transactions, signing keys, and protocol parameters, protecting the integrity of operations.

### How it works

**Step 1**: The user starts both a Cardano node and a Hydra node.  
**Step 2**: The user provides the IP address and host port of the Hydra node to the Kuber Playground.  
**Step 3**: The backend establishes a connection to the Hydra node using a WebSocket connector and updates the frontend with the current head state.  
**Step 4**: The user initializes the Hydra head via the UI in the playground. Signing can be performed either using the user's wallet or a signing key.  
**Step 5**: Once the Hydra head is initialized, the head state becomes visible to the user. This includes details like participants' verification keys, commitments, the head ID, available UTxOs, and the balance within the head.  
**Step 6**: The user creates a transaction in a simple JSON format using the playground interface.  
**Step 7**: The backend validates the transaction based on the availability of UTxOs and the protocol parameters used when running the node.  
**Step 8**: The validated transaction is sent to the frontend, where the user signs it and submits it back to the backend.  
**Step 9**: The backend submits the signed transaction to the Hydra node via the WebSocket connector.  
**Step 10**: After all transactions are completed and the user decides to close the head and perform a fan-out, they can do so through the playground's UI.  
**Step 11**: The backend closes the Hydra head and displays a message indicating whether the transaction can now be fanned out (merged into the main chain) or if there is an error.  
**Step 12**: Once the fan-out is complete, the user can stop their Hydra node.  
