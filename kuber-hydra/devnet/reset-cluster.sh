#!/usr/bin/env bash
set -euo pipefail

docker compose pull                     # pull all images
docker compose down                     # stop and remove any existing containers

if [[ -d runtime ]]; then
	echo "Cleaning up runtime directory"
	docker run --rm -v "$(pwd)/runtime:/target" --entrypoint /bin/bash ghcr.io/intersectmbo/cardano-node:${CARDANO_NODE_VERSION:-10.5.4} -c 'rm -rf /target/*'
fi

bash setup-devnet.sh                    # setup the devnet
bash generate-credentials.sh

docker compose up -d cardano-node

echo "Waiting for cardano-node socket..."
for _ in {1..60}; do
	if [[ -S runtime/node.socket ]]; then
		break
	fi
	sleep 1
done

bash seed-devnet.sh

docker compose up -d