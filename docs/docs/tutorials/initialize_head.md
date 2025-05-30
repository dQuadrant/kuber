# Initialize Hydra Head

```ts
const initResponse = await hydraService.initialize(true);
console.log(initResponse);
```

- The parameter `true` waits indefinitely for a WebSocket response.
- If set to `false`, it waits for 15 seconds and returns a `201 Created` response if no message is received.

**Example Response:**

```json
{
  "headId": "84673dfc0cfd3cf404251fa730fbbfef8d8229b9e2f283e59bca2236",
  "parties": [
    {
      "vkey": "232844b0ebd1f13b62b19bc9ce0a423a84e3d2cc5efa2ac226a96255420d7137"
    },
    {
      "vkey": "f6c153b29e86166552334ccbc5c2995bf5185561a270ff489e98f03a8dd74e7d"
    }
  ],
  "seq": 2,
  "tag": "HeadIsInitializing",
  "timestamp": "2025-05-30T07:29:34.370535241Z"
}
```
