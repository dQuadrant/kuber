## Transaction Metadata with kuber

### 1. Demo using metadata.
```json
{
    "selections":"addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz",
    "outputs": [{
        "address": "addr_test1qqmwpnc72ts9a7fw2trmc2syfy7khtjgrw9vh2cja3psp4lee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafds78dunz",
        "value": "5A"
    }],
    "metadata": {
        "0": {
            "library": "Kuber",
            "version": "2.0.0"
        },
        "1": "Send 5Ada to same address to create collateral"
    }
}
```
[View Resulting transaction](https://testnet.cardanoscan.io/transaction/4e2930a0433e3aeed51a21525f0f389620b51f71c1d3fdaa610ba8da083b6ea1)


### 2. Minting an NFT with metadata