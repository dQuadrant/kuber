Kuber
===========

Haskell library and API server for composing balanced Cardano and Hydra transactions

[KuberIDE.com](https://kuberide.com) is Oficial IDE for plutus development and trying out kuber-server features.

## Docs
 - [Kuber Haskel Library](https://dquadrant.github.io/kuber/haskell_library/)
 - [Transaction Bulder and Ide reference](https://kuberide.com/kuber/docs/intro)


 ## Client library
 Kuber client provides same interface for cardano network and layer2 hydra network.
 - [kuber-client:v3.3.5](https://www.npmjs.com/package/kuber-client/v/3.3.6)

## Hydra
Kuber fully supports hydra transaction building and lifecycle. With `kuber-hydra`, you can interact simultaneously with the layer1 in addition to hydra.
-  Source code: [kuber-hydra](./kuber-hydra)
 - [Hydra docs](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/hydra-js-client/getting-started)

## IDE
 Official IDE with Plutus compilation support is available at [kuberide.com/ide](https://kuberide.com/kuber/login/?state=\%2fide)

## Example Project using Kuber
[cardano-marketplace](https://github.com/dQuadrant/cardano-marketplace)


# Run Kuber-Server with docker-compose

Kuber can be stared easily with [docker-compose.yml](./docker-compose.yml) file. But you will have to wait for cardano-node to sync to latest block

```bash
git clone https://github.com/dquadrant/kuber.git
git checkout v4.0.0
docker-compose up -d
```


If you want to build docker image locally, you can use the helper script
```bash
$ ./.ci/build
```

## Developer guide
Instructions for local development of kuber is available in [DEVELOPER.md](DEVELOPER.md) 
