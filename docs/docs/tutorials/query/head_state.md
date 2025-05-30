# Query Head State

```ts
const headState = await hydraService.queryHeadState();
console.log(headState);
```

```json
{ "state": "Partial Commitments Received" }
```

After the other participant has commited, the head state will display the following:

```json
{ "state": "Open and Ready for Transactions" }
```
