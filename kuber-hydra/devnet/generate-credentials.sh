#!/usr/bin/env bash
set -euo pipefail

# Configuration
NETWORK_ID=42

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BASE_DIR="${SCRIPT_DIR}"
CREDENTIALS_DIR="${BASE_DIR}/credentials"
PARTICIPANTS=("alice" "bob" "carol")

# Create credentials directory
mkdir -p "${CREDENTIALS_DIR}"


CCLI_CMD=
DEVNET_DIR=/devnet
if [[ -n ${1:-} ]]; then
    echo >&2 "Using provided cardano-cli command: ${1}"
    ${1} version > /dev/null
    CCLI_CMD=${1}
    DEVNET_DIR=devnet
fi

HYDRA_NODE_CMD=
if [[ -n ${2:-} ]]; then
    echo >&2 "Using provided hydra-node command: ${2}"
    ${2} --version > /dev/null
    HYDRA_NODE_CMD=${2}
fi

DOCKER_COMPOSE_CMD=
if [[ ! -x ${CCLI_CMD} ]]; then
    if docker compose --version >/dev/null 2>&1; then
        DOCKER_COMPOSE_CMD="docker compose"
    else
        DOCKER_COMPOSE_CMD="docker-compose"
    fi
fi
${DOCKER_COMPOSE_CMD} exec cardano-node mkdir -p /devnet/credentials


# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@}
  else
      ${DOCKER_COMPOSE_CMD} exec cardano-node cardano-cli ${@}
  fi
}

function ccli_() {
  ccli ${@} --testnet-magic ${NETWORK_ID}
}
# Invoke hydra-node in a container or via provided executable
function hnode() {
  if [[ -n ${HYDRA_NODE_CMD} ]]; then
      ${HYDRA_NODE_CMD} ${@}
  else
      docker run --rm -it \
        --pull always \
        -v ${SCRIPT_DIR}:/devnet \
        ghcr.io/cardano-scaling/hydra-node:1.2.0 -- ${@}
  fi
}

generate_cardano_keys() {
    local PREFIX=$1
    echo "Generating Cardano keys: ${PREFIX}"
    ccli address key-gen \
        --verification-key-file ${DEVNET_DIR}/credentials/${PREFIX}.vk \
        --signing-key-file ${DEVNET_DIR}/credentials/${PREFIX}.sk

    ccli address build \
        --payment-verification-key-file ${DEVNET_DIR}/credentials/${PREFIX}.vk \
        --testnet-magic ${NETWORK_ID} \
        --out-file ${DEVNET_DIR}/credentials/${PREFIX}.addr
}

generate_hydra_keys() {
    local PREFIX=$1
    echo "Generating Hydra keys: ${PREFIX}"
    hnode gen-hydra-key --output-file /devnet/credentials/${PREFIX}-hydra
}

# Main
echo "Generating Cardano + Hydra credentials"
echo "Credentials directory:"
echo "  ${CREDENTIALS_DIR}"

for ACTOR in "${PARTICIPANTS[@]}"; do
  echo ""
  echo "----- ${ACTOR} -----"

  # Cardano node wallet
  generate_cardano_keys "${ACTOR}"

  # Hydra head keys
  generate_hydra_keys "${ACTOR}"

  # Funds wallet
  generate_cardano_keys "${ACTOR}-funds"

  echo "Address (${ACTOR}):"
  ${DOCKER_COMPOSE_CMD} exec cardano-node cat /devnet/credentials/${ACTOR}.addr

  echo "Funds Address (${ACTOR}-funds):"
  ${DOCKER_COMPOSE_CMD} exec cardano-node cat /devnet/credentials/${ACTOR}-funds.addr
done

docker cp $(docker compose ps -q cardano-node):/devnet/credentials/ ./
${DOCKER_COMPOSE_CMD} exec cardano-node chown -R $(id -u):$(id -g) /devnet/credentials
echo ""
echo "All credentials generated successfully."
echo ""
echo "Next step:"
echo "  - Run seed-faucet script to fund addresses"