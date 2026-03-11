Initial Stage: -[ ] query state (is giving error)

```
Connected to Hydra WebSocket server.
Forwarded: {"headStatus":"Initializing","hydraHeadId":"30979d2026346646992c3b1ce6bc842c2aac43cdc84e22f232c0ac1d","hydraNodeVersion":"0.22.0-65b4c1201e7533d4269d42e2ca1c8b53f999454f","me":{"vkey":"a2347e0e1f6276aa3b0b33984dbef86d37f25d6ff9b8d4e123ac3f4540f448b1"},"snapshotUtxo":{},"tag":"Greetings"}

Forwarded: {"tag": "GetUTxO"}
Forwarded: {"input":"{\"tag\": \"GetUTxO\"}","reason":"Error in $.tag: parsing Hydra.API.ClientInput.ClientInput failed, expected tag field to be one of [\"Init\",\"Abort\",\"NewTx\",\"Recover\",\"Decommit\",\"Close\",\"Contest\",\"Fanout\",\"SideLoadSnapshot\"], but found tag \"GetUTxO\""}
```

Notes of hydra state:

1.  When nodes are started but no initialized

```json
{"contents":{"chainState":{"recordedAt":null,"spendableUTxO":{}}},"tag":"Idle"}
```

2.  When initialized

```json
{
  "contents": {
    "chainState": {
      "recordedAt": {
        "blockHash": "d61dc384c2c1f63b732cca0544e021268339ad283236ef683f10df82635dd740",
        "slot": 84709736,
        "tag": "ChainPoint"
      },
      "spendableUTxO": {
        "9166efe7ad80e3ba7c6774fe18b19ae4939edc30e6087a3569cbd92497d89e44#0": {
          "address": "addr_test1wzlxa0r5ggyvvc9lplwpe7a4z4680nfsthjmzam72awtknqz5cf7v",
          "datum": null,
          "inlineDatum": {
            "constructor": 0,
            "fields": [
              { "constructor": 0, "fields": [{ "int": 600000 }] },
              {
                "list": [
                  {
                    "bytes": "bc1067a077fc148dfc069528c6ce0c54d5c4b7ddcc8a1683da7a9ce023aaa949"
                  },
                  {
                    "bytes": "c2eb9e237e1930ca1ffcdb3ca15d54a4a19897a1e51dc79e1aa21130a1da616c"
                  }
                ]
              },
              {
                "bytes": "9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72"
              },
              {
                "constructor": 0,
                "fields": [
                  {
                    "bytes": "126ebf0678246137b58aa2d87d0bf9f6b77bd42416b6242d1e74fc57fd3b2e7a"
                  },
                  { "int": 3 }
                ]
              }
            ]
          },
          "inlineDatumRaw": "d8799fd8799f1a000927c0ff9f5820bc1067a077fc148dfc069528c6ce0c54d5c4b7ddcc8a1683da7a9ce023aaa9495820c2eb9e237e1930ca1ffcdb3ca15d54a4a19897a1e51dc79e1aa21130a1da616cff581c9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72d8799f5820126ebf0678246137b58aa2d87d0bf9f6b77bd42416b6242d1e74fc57fd3b2e7a03ffff",
          "inlineDatumhash": "b478e3f2803582b24c9469d260a46d1aaefdaccb55c2c352bcd7be312c8f1cbf",
          "referenceScript": null,
          "value": {
            "9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72": {
              "4879647261486561645631": 1
            },
            "lovelace": 1741240
          }
        },
        "9166efe7ad80e3ba7c6774fe18b19ae4939edc30e6087a3569cbd92497d89e44#1": {
          "address": "addr_test1wry2zqd9ezkys94smn44nn33ls393cu8m6pg7q5kr5hjq3g2tzley",
          "datum": null,
          "inlineDatum": {
            "bytes": "9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72"
          },
          "inlineDatumRaw": "581c9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72",
          "inlineDatumhash": "0a4a756718a5ca29689db7f4ebb7eeeaaee8fb8e93dc3454ef01a1b35f716f1c",
          "referenceScript": null,
          "value": {
            "9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72": {
              "5bb231d8255f272adddef07ec92913a55af7a65899c975aa95073f01": 1
            },
            "lovelace": 1293000
          }
        },
        "9166efe7ad80e3ba7c6774fe18b19ae4939edc30e6087a3569cbd92497d89e44#2": {
          "address": "addr_test1wry2zqd9ezkys94smn44nn33ls393cu8m6pg7q5kr5hjq3g2tzley",
          "datum": null,
          "inlineDatum": {
            "bytes": "9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72"
          },
          "inlineDatumRaw": "581c9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72",
          "inlineDatumhash": "0a4a756718a5ca29689db7f4ebb7eeeaaee8fb8e93dc3454ef01a1b35f716f1c",
          "referenceScript": null,
          "value": {
            "9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72": {
              "f697b774e28f5d56fb2ff58a3a7c55fc9fbe1e72bd129f194bd56802": 1
            },
            "lovelace": 1293000
          }
        },
        "9166efe7ad80e3ba7c6774fe18b19ae4939edc30e6087a3569cbd92497d89e44#3": {
          "address": "addr_test1vpdmyvwcy40jw2kammc8ajffzwj44aaxtzvujad2j5rn7qgmccqgu",
          "datum": null,
          "datumhash": null,
          "inlineDatum": null,
          "inlineDatumRaw": null,
          "referenceScript": null,
          "value": { "lovelace": 23707903 }
        },
        "f8d24066625b50272ea1478b8948278a74199bdebb945c72e472510d06256979#0": {
          "address": "addr_test1wzlxa0r5ggyvvc9lplwpe7a4z4680nfsthjmzam72awtknqz5cf7v",
          "datum": null,
          "inlineDatum": {
            "constructor": 2,
            "fields": [
              {
                "constructor": 0,
                "fields": [
                  {
                    "bytes": "6a436ae6f8ef57b197c31e512af034302068f58d82491c65d607919e"
                  },
                  {
                    "list": [
                      {
                        "bytes": "d5bf4a3fcce717b0388bcc2749ebc148ad9969b23f45ee1b605fd58778576ac4"
                      }
                    ]
                  },
                  { "constructor": 0, "fields": [{ "int": 200000 }] },
                  { "int": 0 },
                  { "int": 0 },
                  {
                    "bytes": "712ea382d0f786c8066024440bf2fb3b39c6c32d81023d150fb7745e711a8db0"
                  },
                  {
                    "bytes": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                  },
                  {
                    "bytes": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                  },
                  { "list": [] },
                  { "int": 1751365886000 }
                ]
              }
            ]
          },
          "inlineDatumRaw": "d87b9fd8799f581c6a436ae6f8ef57b197c31e512af034302068f58d82491c65d607919e9f5820d5bf4a3fcce717b0388bcc2749ebc148ad9969b23f45ee1b605fd58778576ac4ffd8799f1a00030d40ff00005820712ea382d0f786c8066024440bf2fb3b39c6c32d81023d150fb7745e711a8db05820e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b8555820e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855801b00000197c58aa030ffff",
          "inlineDatumhash": "2892e6317ad08f0977702725a8b00de99d65bd53b9e685fb15e3ff10799645fb",
          "referenceScript": null,
          "value": {
            "6a436ae6f8ef57b197c31e512af034302068f58d82491c65d607919e": {
              "4879647261486561645631": 1,
              "f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d": 1
            },
            "lovelace": 28852547
          }
        },
        "f8d24066625b50272ea1478b8948278a74199bdebb945c72e472510d06256979#1": {
          "address": "addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3",
          "datum": null,
          "datumhash": null,
          "inlineDatum": null,
          "inlineDatumRaw": null,
          "referenceScript": null,
          "value": { "lovelace": 44475579 }
        }
      }
    },
    "committed": {},
    "headId": "9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72",
    "headSeed": "2231323665626630363738323436313337623538616132643837643062663966366237376264343234313662363234326431653734666335376664336232653761233322",
    "parameters": {
      "contestationPeriod": 600,
      "parties": [
        {
          "vkey": "bc1067a077fc148dfc069528c6ce0c54d5c4b7ddcc8a1683da7a9ce023aaa949"
        },
        {
          "vkey": "c2eb9e237e1930ca1ffcdb3ca15d54a4a19897a1e51dc79e1aa21130a1da616c"
        }
      ]
    },
    "pendingCommits": [
      {
        "vkey": "bc1067a077fc148dfc069528c6ce0c54d5c4b7ddcc8a1683da7a9ce023aaa949"
      },
      {
        "vkey": "c2eb9e237e1930ca1ffcdb3ca15d54a4a19897a1e51dc79e1aa21130a1da616c"
      }
    ]
  },
  "tag": "Initial"
}
```


3. When committed and is open

```json
{"contents":{"chainState":{"recordedAt":{"blockHash":"52bb23a13fefbecf2752cbee0e648d64577db8e5023b987e6209ab3b0e80d417","slot":84711122,"tag":"ChainPoint"},"spendableUTxO":{"d6050aff854c48d9f2fa3b8e15c2293fbe66d893e9598d1d73f671a84c4e37f7#0":{"address":"addr_test1wzlxa0r5ggyvvc9lplwpe7a4z4680nfsthjmzam72awtknqz5cf7v","datum":null,"inlineDatum":{"constructor":1,"fields":[{"constructor":0,"fields":[{"bytes":"9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72"},{"list":[{"bytes":"bc1067a077fc148dfc069528c6ce0c54d5c4b7ddcc8a1683da7a9ce023aaa949"},{"bytes":"c2eb9e237e1930ca1ffcdb3ca15d54a4a19897a1e51dc79e1aa21130a1da616c"}]},{"constructor":0,"fields":[{"int":600000}]},{"int":0},{"bytes":"cd992dab932d47b7a49b3fc2f91171d4f06f7f680e956a9d0ea733f258612eaa"}]}]},"inlineDatumRaw":"d87a9fd8799f581c9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d729f5820bc1067a077fc148dfc069528c6ce0c54d5c4b7ddcc8a1683da7a9ce023aaa9495820c2eb9e237e1930ca1ffcdb3ca15d54a4a19897a1e51dc79e1aa21130a1da616cffd8799f1a000927c0ff005820cd992dab932d47b7a49b3fc2f91171d4f06f7f680e956a9d0ea733f258612eaaffff","inlineDatumhash":"c3518b37a47bdad91ad05582edb321f372914f5405fdabc5bacbbd52d0a24dd1","referenceScript":null,"value":{"9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72":{"4879647261486561645631":1,"5bb231d8255f272adddef07ec92913a55af7a65899c975aa95073f01":1,"f697b774e28f5d56fb2ff58a3a7c55fc9fbe1e72bd129f194bd56802":1},"lovelace":64327240}},"d6050aff854c48d9f2fa3b8e15c2293fbe66d893e9598d1d73f671a84c4e37f7#1":{"address":"addr_test1vpdmyvwcy40jw2kammc8ajffzwj44aaxtzvujad2j5rn7qgmccqgu","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":19970159}},"d9c2aa9bf5a07866a0c0e803587a1863d6aec1ca33c19a4a477a4b526e08c375#0":{"address":"addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":25964847}},"d9c2aa9bf5a07866a0c0e803587a1863d6aec1ca33c19a4a477a4b526e08c375#1":{"address":"addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":45196531}},"eb96fc301c0822d0861858729d2bda86691a31a5321552c16726689519eabd05#1":{"address":"addr_test1vrmf0dm5u28464hm9l6c5wnu2h7fl0s7w27398cef02ksqsen7ca9","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":28226143}},"ff801dd680a49edb3c82ab95ab17f1a7e08940a786f217eb219013e33020fc06#0":{"address":"addr_test1wzhqrkk782wrgm2ujwhreceegy4epg9clqlefmrt4gjwxrqckxj2j","datum":null,"inlineDatum":{"constructor":0,"fields":[{"bytes":"aaa80297c2d1c6aee22caeee13eaff16e9fcaae109de97320b3c758c"},{"int":1751367795313},{"list":[{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"293c619d341d4852c86ff5aee2a13bca4c1cbcde941128dd6e6c78fc533c9378"},{"int":0}]},{"bytes":"d8799fd8799fd8799f581cd4c8023d6fafd36f824505258dd4dcaed23ae76a6a6588385b97d2fdffd87a80ffa140a1401a000f4240d87980d87a80ff"}]}]}]},"inlineDatumRaw":"d8799f581caaa80297c2d1c6aee22caeee13eaff16e9fcaae109de97320b3c758c1b00000197c5a7c2719fd8799fd8799f5820293c619d341d4852c86ff5aee2a13bca4c1cbcde941128dd6e6c78fc533c937800ff583cd8799fd8799fd8799f581cd4c8023d6fafd36f824505258dd4dcaed23ae76a6a6588385b97d2fdffd87a80ffa140a1401a000f4240d87980d87a80ffffffff","inlineDatumhash":"c2c9b90f51cb39e1db45251f5b2ce15f68a586e85e18462d0c709f2faa703e7a","referenceScript":null,"value":{"lovelace":1534360}},"ff801dd680a49edb3c82ab95ab17f1a7e08940a786f217eb219013e33020fc06#1":{"address":"addr_test1vp0vlzdlpcqw47rx0k0lnwujcma2ctaucmlhrn5634k9qvcx3xrlc","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":737098424}}}},"coordinatedHeadState":{"allTxs":{},"confirmedSnapshot":{"headId":"9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72","initialUTxO":{"126ebf0678246137b58aa2d87d0bf9f6b77bd42416b6242d1e74fc57fd3b2e7a#0":{"address":"addr_test1vrksrm7gfqkz53h3akapxaezpdjan7pw5wvcepc6xjeg32qrttfhn","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":30000000}},"126ebf0678246137b58aa2d87d0bf9f6b77bd42416b6242d1e74fc57fd3b2e7a#2":{"address":"addr_test1vzd5vhzyd4y4dqm85979dkq0j040sfkxw6rt3nt8wj8c5jqmenq20","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":30000000}}},"tag":"InitialSnapshot"},"currentDepositTxId":null,"decommitTx":null,"localTxs":[],"localUTxO":{"126ebf0678246137b58aa2d87d0bf9f6b77bd42416b6242d1e74fc57fd3b2e7a#0":{"address":"addr_test1vrksrm7gfqkz53h3akapxaezpdjan7pw5wvcepc6xjeg32qrttfhn","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":30000000}},"126ebf0678246137b58aa2d87d0bf9f6b77bd42416b6242d1e74fc57fd3b2e7a#2":{"address":"addr_test1vzd5vhzyd4y4dqm85979dkq0j040sfkxw6rt3nt8wj8c5jqmenq20","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":30000000}}},"pendingDeposits":{},"seenSnapshot":{"tag":"NoSeenSnapshot"},"version":0},"currentSlot":84711127,"headId":"9b6cc05367a84d66241c39c503516ad7d47fb2668fcbd9bc0e993d72","headSeed":"2231323665626630363738323436313337623538616132643837643062663966366237376264343234313662363234326431653734666335376664336232653761233322","parameters":{"contestationPeriod":600,"parties":[{"vkey":"bc1067a077fc148dfc069528c6ce0c54d5c4b7ddcc8a1683da7a9ce023aaa949"},{"vkey":"c2eb9e237e1930ca1ffcdb3ca15d54a4a19897a1e51dc79e1aa21130a1da616c"}]}},"tag":"Open"}
```