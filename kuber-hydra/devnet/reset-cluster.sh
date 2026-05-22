#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

docker compose pull                     # pull all images
docker compose down --volumes                     # stop and remove any existing containers

if [[ -d runtime ]]; then
	echo "Cleaning up runtime directory"
	docker run --rm -v "$(pwd)/runtime:/target" --entrypoint /bin/bash ghcr.io/intersectmbo/cardano-node:${CARDANO_NODE_VERSION:-11.0.1} -c 'rm -rf /target/*'
fi

bash setup-devnet.sh                    # setup the devnet
bash generate-credentials.sh

if [[ ! -f runtime/cardano-node/cardano-node.json ]]; then
	echo "Missing runtime/cardano-node/cardano-node.json after setup; aborting." >&2
	exit 1
fi

# Ensure Docker can see the bind-mounted runtime before starting cardano-node.
for _ in {1..20}; do
	if docker run --rm -v "$(pwd)/runtime:/mnt" alpine sh -c 'test -f /mnt/cardano-node/cardano-node.json' >/dev/null 2>&1; then
		break
	fi
	sleep 1
done
docker run --rm -v "$(pwd)/runtime:/mnt" alpine sh -c 'test -f /mnt/cardano-node/cardano-node.json' >/dev/null 2>&1 \
	|| { echo "Runtime bind mount not visible to Docker; aborting." >&2; exit 1; }

docker compose up -d cardano-node

echo "Waiting for cardano-node to answer local queries..."
NODE_READY=false
for _ in {1..120}; do
	if docker compose ps --status running --services | grep -qx cardano-node \
		&& docker compose exec -T cardano-node cardano-cli conway query tip \
			--testnet-magic 42 \
			--socket-path /devnet/runtime/node.socket >/dev/null 2>&1; then
		NODE_READY=true
		break
	fi
	sleep 1
done

if [[ "${NODE_READY}" != "true" ]]; then
	echo "cardano-node did not become ready in time" >&2
	docker compose ps >&2
	docker compose logs --tail=80 cardano-node >&2
	exit 1
fi

bash seed-devnet.sh

docker compose up -d
