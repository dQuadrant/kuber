#!/usr/bin/env bash

#if directory exists, it will be removed 

set -eo pipefail

BASEDIR=${BASEDIR:-$(realpath $(dirname $(realpath $0))/..)}
TARGETDIR=${TARGETDIR:-runtime}


if [ -d "$TARGETDIR" ]; then
  echo "Cleaning up directory $TARGETDIR"
  # Use docker to delete as root to handle any permission issues
  docker run --rm -v "$(pwd)/$TARGETDIR:/target" alpine sh -c 'rm -rf /target/*'
  rm -rf "$TARGETDIR" 2>/dev/null || true
fi


sed -i.bak "s/\"startTime\": [0-9]*/\"startTime\": $(date +%s)/" "cardano-node/genesis-byron.json" && \
sed -i.bak "s/\"systemStart\": \".*\"/\"systemStart\": \"$(date -u +%FT%TZ)\"/" "cardano-node/genesis-shelley.json"

chmod 600 cardano-node/faucet.sk
chmod 600 cardano-node/faucet.vk
chmod 600 cardano-node/kes.skey
chmod 600 cardano-node/vrf.skey

if [ ! -f .env ]; then
    cat > .env << 'EOF'
HYDRA_SCRIPTS_TX_ID=
EOF
    
fi
echo "Prepared devnet, you can start the cluster now"

