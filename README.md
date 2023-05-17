Kuber
===========

Haskell library and API server for composing balanced Cardano transactions.


**Notice:** Playground for kuber has been moved to [dquadrant/kuber-playground](https://dquadrant.github.io/kuber-playgrounnd/)



## Docs
 - [JSON API](https://kuberide.com/kuber/docs/intro)
 - [Kuber Haskel Library](https://dquadrant.github.io/kuber)

## Example Project using Kuber (v2)
[cardano-marketplace](https://github.com/dQuadrant/cardano-marketplace)


###  Kuber Kontract usage Example
```haskell

remoteKuberConnection :: IO RemoteKuberConnection
remoteKuberConnection = do 
    (networkName,network) <- getNetworkFromEnv "NETWORK"    
    createRemoteKuberConnection network "http://localhost:8081" Nothing

localNodeConnection :: IO ChainConnectInfo
localNodeConnection = chainInfoFromEnv

printBalanceKontract :: HasChainQueryAPI api =>  Kontract api w FrameworkError ()
printBalanceKontract=  do
    addr <- kWrapParser $ parseAddressBech32 (T.pack "addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj")
    tip <- kQueryChainPoint
    utxos <- kQueryUtxoByAddress (Set.singleton $ addressInEraToAddressAny addr)
    liftIO $ do 
        putStrLn ("Chain is at " ++ (case tip of
            ChainPointAtGenesis -> "Genesis"
            ChainPoint (SlotNo sn) ha -> "SlotNo:" ++ show sn ++ " BlockHeaderHash:" ++ T.unpack (serialiseToRawBytesHexText ha)) )
        putStrLn  $ "Your utxos : \n" ++ toConsoleText " - " utxos

main :: IO ()
main = do
  kuberConn <- localNodeConnection
  --  kuberConn <- remoteKuberConnection
  result <- evaluateKontract  kuberConn printBalanceKontract
  case result of 
    Left e -> putStrLn $ "Unexpected error evaluating printBalance kontract:\n  "++ show e
    _ -> pure ()
```

# Run Kuber-Server with docker-compose

Kuber can be stared easily with [docker-compose.yml](./docker-compose.yml) file. But you will have to wait for cardano-node to sync to latest block

```bash
git clone https://github.com/dquadrant/kuber.git
git checkout 3.0.0-rc1
docker-compose up -d
```


If you want to build docker image locally, you can use the helper script
```bash
$ ./.ci/build
```


## Developer guide
Instructions for local development is available in [DEVELOPER.md](DEVELOPER.md) 
