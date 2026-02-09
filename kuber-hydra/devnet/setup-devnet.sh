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

mkdir -p "$TARGETDIR"
mkdir -p "credentials"
cp -a cardano-node "$TARGETDIR/"

update_json_in_place() {
  local file="$1"
  local pattern="$2"
  local replacement="$3"
  local tmp_file

  tmp_file="$(mktemp)"
  sed -e "s|$pattern|$replacement|" "$file" > "$tmp_file"
  mv "$tmp_file" "$file"
}

update_json_in_place \
  "$TARGETDIR/cardano-node/genesis-byron.json" \
  "\"startTime\": [0-9]*" \
  "\"startTime\": $(date +%s)"

update_json_in_place \
  "$TARGETDIR/cardano-node/genesis-shelley.json" \
  "\"systemStart\": \".*\"" \
  "\"systemStart\": \"$(date -u +%FT%TZ)\""

chmod 600 "$TARGETDIR/cardano-node/faucet.sk"
chmod 600 "$TARGETDIR/cardano-node/faucet.vk"
chmod 600 "$TARGETDIR/cardano-node/kes.skey"
chmod 600 "$TARGETDIR/cardano-node/vrf.skey"

if [ ! -f .env ]; then
    cat > .env << 'EOF'
HYDRA_SCRIPTS_TX_ID=
EOF
    
fi
echo "Prepared devnet, you can start the cluster now"

