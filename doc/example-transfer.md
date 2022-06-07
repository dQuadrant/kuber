## Spending Ada and token 

### 1. Transfering ada to different addresses
```json
{
    "selections":["addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj"],
    "outputs": [{
        "address": "addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz",
        "value": "10A"
    },{
      "address": "addr_test1qzq2uxdnhnrk2rsd6tpr5nku9zmkc3aqc8kdtk3r0zsy9e0ee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdsxan2r6",
      "value": 20000000
    }]
}
```
[View Resulting transaction](https://testnet.cardanoscan.io/transaction/ce15765c0a5ee850905cf576723ef80d9489270b44a952ad748dfa58c4a93973)

### 2. Transferring assets
```json
{
    "selections": ["addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz"],
    "outputs": [{
      "address": "addr_test1qzq2uxdnhnrk2rsd6tpr5nku9zmkc3aqc8kdtk3r0zsy9e0ee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdsxan2r6",
      "value": "10A + 1000 29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6.MINt"
    }]
}
```
[View Resulting transaction](https://testnet.cardanoscan.io/transaction/43dbfaf7ada3b123cd4009377cb9dd13a90eead66032fe7c19ba23d32f43eefb)

### 3. Using  deductFee option
**Note**  when using 'deduct fee' option, we only lose the exact amount we intended to spend. The fee is paid by the receiver.

```json
{
    "selections": ["addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz"],
    "outputs": [{
      "address": "addr_test1qzq2uxdnhnrk2rsd6tpr5nku9zmkc3aqc8kdtk3r0zsy9e0ee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdsxan2r6",
      "value": "10A",
      "deductFee": true
    }]
}
```
[View Resulting transaction](https://testnet.cardanoscan.io/transaction/7d56470f3f7f53a7e4332dff77811ef1e9bd0f89413ad388ab305be39e6c0780)