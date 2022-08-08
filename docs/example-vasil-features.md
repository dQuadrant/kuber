**Inline Datum**
Inline datum is a newly introduced feature after vasil hardfork. It basically allows datum to be attached to outputs instead of datum hashes which allows much simpler communication of datum values between users. You can use inline datum feature with placing the required datum in "datum" field and making "inline" true.
Read more about it here
https://developers.cardano.org/docs/governance/cardano-improvement-proposals/cip-0032/
```json
{
    "selections" :["addr_test1vq66k7sxqef27nwkr4htffs6lgr4pyg5aut4s7ugfclqt5qgxpnz9"],
    "outputs": [
        {
            "address": "addr_test1wq40nfjurthn364rvhhzu2y8z3t0d53s2z9sz0303hya3us9a490f",
            "value": "2A + 7ace8cc4f17b0e494af8f9f5a4936e55e1fbc6759b0a1ca2dee7ac08.mytoken",
            "datum": {"fields":[{"fields":[{"fields":[{"bytes":"35ab7a060652af4dd61d6eb4a61afa07509114ef17587b884e3e05d0"}],"constructor":0},{"fields":[],"constructor":1}],"constructor":0},{"int":2000000}],"constructor":0},
            "inline": true
        }
    ]
}
```
**Reference Script**


**Reference Input**
