# Query UTxOs

```ts
const utxoResponse = await hydraService.queryUTxOByAddress(
  "addr_test1vqvz4mh9z9qeltx5ha82kafcrpeg3f26vvl40xlqeumgj7cugpfje"
);
console.log(utxoResponse);
```
This will return a UTxO type from the libcardano package.
