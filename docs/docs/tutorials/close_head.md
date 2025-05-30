# Closing the Hydra Head

```ts
const closeResponse = await hydraService.close(true);
console.log(closeResponse);
```

**Example Response:**

```json
{
  "contestationDeadline": "2025-05-30T08:11:34Z",
  "headId": "84673dfc0cfd3cf404251fa730fbbfef8d8229b9e2f283e59bca2236",
  "seq": 17,
  "snapshotNumber": 1,
  "tag": "HeadIsClosed",
  "timestamp": "2025-05-30T08:07:54.34265662Z"
}
```

> 🛠 If you encounter errors on closing, refer to: [Hydra Issue #1039](https://github.com/cardano-scaling/hydra/issues/1039)