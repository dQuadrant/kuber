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
    :
fi


# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@}
  else
      docker run --rm -it \
          --user "$(id -u):$(id -g)" \
          --workdir /devnet \
          -v ${SCRIPT_DIR}:/devnet \
          --entrypoint="sh" \
          ghcr.io/intersectmbo/cardano-node:${CARDANO_NODE_VERSION:-10.5.4} \
          -c 'cardano-cli "$@"' sh "$@"
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
                --user "$(id -u):$(id -g)" \
        --workdir /devnet \
        -v ${SCRIPT_DIR}/:/devnet/ \
      ghcr.io/cardano-scaling/hydra-node:1.2.0 \
      ${@}
  fi
}

generate_cardano_keys() {
    local PREFIX=$1
    local vk_path="${DEVNET_DIR}/credentials/${PREFIX}.vk"
    local sk_path="${DEVNET_DIR}/credentials/${PREFIX}.sk"
    local addr_path="${DEVNET_DIR}/credentials/${PREFIX}.addr"
    local host_vk_path="${CREDENTIALS_DIR}/${PREFIX}.vk"
    local host_sk_path="${CREDENTIALS_DIR}/${PREFIX}.sk"
    local host_addr_path="${CREDENTIALS_DIR}/${PREFIX}.addr"

    echo "Generating Cardano keys: ${PREFIX}"

    if [[ -f "${host_vk_path}" || -f "${host_sk_path}" ]]; then
        echo "  Skipping key generation; keys already exist."
    else
        ccli address key-gen \
            --verification-key-file "${vk_path}" \
            --signing-key-file "${sk_path}"
    fi

    if [[ -f "${host_addr_path}" ]]; then
        echo "  Skipping address build; address already exists."
    else
        ccli address build \
            --payment-verification-key-file "${vk_path}" \
            --testnet-magic "${NETWORK_ID}" \
            --out-file "${addr_path}"
    fi
}

generate_hydra_keys() {
    local PREFIX=$1
    local host_vk_path="${CREDENTIALS_DIR}/${PREFIX}-hydra.vk"
    local host_sk_path="${CREDENTIALS_DIR}/${PREFIX}-hydra.sk"

    echo "Generating Hydra keys: ${PREFIX}"
    if [[ -f "${host_vk_path}" || -f "${host_sk_path}" ]]; then
        echo "  Skipping Hydra key generation; keys already exist."
    else
        hnode gen-hydra-key --output-file /devnet/credentials/${PREFIX}-hydra
    fi
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
    cat "${CREDENTIALS_DIR}/${ACTOR}.addr"

  echo "Funds Address (${ACTOR}-funds):"
    cat "${CREDENTIALS_DIR}/${ACTOR}-funds.addr"
done
echo ""
echo "All credentials generated successfully."
echo ""
echo "Next step:"
echo "  - Run seed-faucet script to fund addresses"