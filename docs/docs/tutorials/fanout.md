
# Fanout Transactions to Mainnet

After the contestation period, finalize and fanout:

```ts
const response = await hydraService.fanout(true);
console.log(response);
```

**Example Response:**

```json
{
  "headId": "84673dfc0cfd3cf404251fa730fbbfef8d8229b9e2f283e59bca2236",
  "seq": 22,
  "tag": "HeadIsFinalized",
  "timestamp": "2025-05-30T08:15:34.750763178Z",
  "utxo": {
    "4051dd270c1a51da8645b6c91d23053273547f1f853929fbec5519527e18266d#4": {
      "address": "addr_test1vqvf72x6j3sr2jtr5p2yu2jr9emzkxpv0gw859yde8kttqsu0vlpf",
      "datum": null,
      "datumhash": null,
      "inlineDatum": null,
      "referenceScript": null,
      "value": {
        "lovelace": 50000000
      }
    },
    "ff6de11af9d0998a85c3eb5333f4afd173a904164630fc0717be8ee819900d4f#0": {
      "address": "addr_test1vqvz4mh9z9qeltx5ha82kafcrpeg3f26vvl40xlqeumgj7cugpfje",
      "datum": null,
      "inlineDatum": {
        "constructor": 2,
        "fields": []
      },
      "inlineDatumhash": "ff5f5c41a5884f08c6e2055d2c44d4b2548b5fc30b47efaa7d337219190886c5",
      "referenceScript": null,
      "value": {
        "lovelace": 2000000
      }
    },
    "ff6de11af9d0998a85c3eb5333f4afd173a904164630fc0717be8ee819900d4f#1": {
      "address": "addr_test1vqvz4mh9z9qeltx5ha82kafcrpeg3f26vvl40xlqeumgj7cugpfje",
      "datum": null,
      "inlineDatum": {
        "constructor": 2,
        "fields": []
      },
      "inlineDatumhash": "ff5f5c41a5884f08c6e2055d2c44d4b2548b5fc30b47efaa7d337219190886c5",
      "referenceScript": null,
      "value": {
        "lovelace": 2000000
      }
    },
    "ff6de11af9d0998a85c3eb5333f4afd173a904164630fc0717be8ee819900d4f#2": {
      "address": "addr_test1vqvz4mh9z9qeltx5ha82kafcrpeg3f26vvl40xlqeumgj7cugpfje",
      "datum": null,
      "datumhash": null,
      "inlineDatum": null,
      "referenceScript": null,
      "value": {
        "lovelace": 16000000
      }
    }
  }
}
```