
# Hydra Integration Project Plan for Kuber

**Repository:** [https://github.com/dquadrant/kuber](https://github.com/dquadrant/kuber)

----------

## Summary

This project integrates **Kuber**, a Haskell-based Cardano Layer 1 transaction builder, with **Hydra**, Cardano’s Layer 2 scalability solution.
Kuber simplifies transaction construction by allowing developers to define transactions in JSON, abstracting away complex tasks like fee balancing and submission.
The goal is to extend this user-friendly transaction interface to also support the Hydra protocol, enabling developers to submit Layer 2 transactions just as easily as Layer 1.

Kuber already supports full functionality for Layer 1, including transaction construction, validation, and submission.
This integration will bring the same functionality to Hydra, allowing developers to interact with a Hydra Head as easily as with Layer 1.

----------

## Scope of Project

-   Extend Kuber to support building, validating, and submitting transactions over a Hydra Head.
-   Implement Hydra lifecycle operations via Kuber API, including initializing the head, committing UTxOs, closing, aborting, and fanout.
-   Provide well-structured WebSocket proxy support to receive and interpret Hydra events.
-   Ensure Kuber works seamlessly with both **mainnet** and **testnet** deployments.
-   Provide a client library to easily integrate with the backend

----------

#  Milestones

## M1 Project Planning and Architecture
### Deliverables
- Deliverable includes this document and project architecture details
- Details are published to github : [Architecture.md](./Architecture.md)

## M2.1 - Core Hydra Functionalities (Haskell, kuber-server)

### Deliverables

**2.1.1** Hydra WebSocket Integration – Kuber connects to and parses Hydra WebSocket event stream  
**2.1.2** Query API Implementation – REST APIs for querying UTXOs, state, protocol parameters  
**2.1.3** Transaction Build Support – JSON-based builder adjusted for Hydra; validation and balancing  
**2.1.4** Hydra Lifecycle Relay API – Endpoints for init, commit, decommit, close, contest, abort, fanout  
**2.1.5** Finalization & Documentation – Markdown docs, GitHub issues, testing, demo setup



### M2.2 - Browser/Nodejs suppport (kuber-client)

#### Features
- Implementation of Hydra functionalities in JavaScript for the npm package.
- Ability to listen and react to events/transactions on hydra node.
- Submit transactions to hydra node and get transaction status.
- Compatibility with DApps, including wallet connection for transaction signing.
- Extensive test coverage for the npm package.

#### Deliverables
**2.2.1** Verification of compatibility with existing Kuber/Hydra APIs.
**2.2.2** Client Package - Published npm package and test results in the repository.


### M3: Documentation and Community Engagement

### Todos
- Comprehensive documentation for the Hydra-enabled Kuber library, Kuber Playground, and Kuber-Client npm package.
- Tutorials and guides for developers to integrate Hydra features into their applications.
a
### Deliverables
- Docusaurus deploymennt with docs and tutorials.

----------

## Risks and Issues

-   **Hydra WebSocket Latency**:
    -   Submissions and state transitions may have slow response times.
-   **Non-standard API Responses**:
    -   Some requests (e.g., transaction submission) may result in a `201 Created` instead of `200 OK`, due to asynchronous handling.
-   **Hydra Node Setup**:
    -   Kuber does not manage or deploy Hydra nodes; users must set them up and configure connectivity manually.


# 🗓️ Hydra Integration Project Timeline

This document outlines the timeline for the Hydra integration into the Kuber platform.

---

| **Phase** | **Milestone / Task** | **Estimated Duration** | **Target Dates** | **Deliverables** |
|-----------|----------------------|------------------------|------------------|------------------|
| 🔹 M1 | **Project Planning & Architecture** | 1 week | Apr 12 – Apr 19 | Architecture document, project setup on GitHub |
| 🔹 M2.1 | **Hydra WebSocket Integration** | 1.5 weeks | Apr 21 – May 2 | Hydra WS connection, event parsing |
|  | **Query API Implementation** | 1 week | May 3 – May 10 | REST APIs for UTXO, state, protocol params |
|  | **Hydra Transaction Build Support** | 3 weeks | May 11 – May 31 | JSON builder for Hydra, validation, balancing |
|  | **Lifecycle Relay API Endpoints** | 1 week | Jun 1 – Jun 7 | API: init, commit, close, abort, fanout, etc. |
|  | **Finalization & Documentation** | 1 week | Jun 8 – Jun 14 | Markdown docs, issue tracking, test/demo |
| 🔹 M2.2 | **Client-side Hydra Support (npm package)** | 3 weeks | Jun 15 – Jul 5 | JS package: listen, submit, test, publish |
|  | **npm Package Publication** | 2 weeks | Jul 6 – Jul 19 | Verified build, published to npm, repo updated |
| 🔹 M3 | **Documentation & Community Engagement** | 2 weeks | Jul 20 – Aug 3 | Docusaurus deployment, tutorials, examples |
| **—** | **🧮 Total Estimated Duration** | **15 weeks** | **Apr 12 – Aug 3, 2025** | — |

---

