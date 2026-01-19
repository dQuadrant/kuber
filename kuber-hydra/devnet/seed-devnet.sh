#!/usr/bin/env bash

# Distributing ada to hydra nodes in a single transaction
set -eo pipefail
# See: https://github.com/cardano-scaling/hydra/pull/1682
#Tests if jq can handle 64-bit integers without corruption
[[ $(jq -n '9223372036854775807') == "9223372036854775807" ]] \
  || (echo "bad jq roundtrip: please upgrade your jq to version 1.7+"; exit 1)

SCRIPT_DIR=${SCRIPT_DIR:-$(realpath $(dirname $(realpath $0)))}
NETWORK_ID=42

CCLI_CMD=
DEVNET_DIR=/devnet
if [[ -n ${1} ]]; then
    echo >&2 "Using provided cardano-cli command: ${1}"
    $(${1} version > /dev/null)
    CCLI_CMD=${1}
    DEVNET_DIR=devnet
fi

HYDRA_NODE_CMD=
if [[ -n ${2} ]]; then
    echo >&2 "Using provided hydra-node command: ${2}"
    ${2} --version > /dev/null
    HYDRA_NODE_CMD=${2}
fi

DOCKER_COMPOSE_CMD=
if [[ ! -x ${CCLI_CMD} ]]; then
  if docker compose --version > /dev/null 2>&1; then
    DOCKER_COMPOSE_CMD="docker compose"
  else
    DOCKER_COMPOSE_CMD="docker-compose"
  fi
fi

# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@} --testnet-magic ${NETWORK_ID}
  else
      ${DOCKER_COMPOSE_CMD} exec -T cardano-node cardano-cli ${@} --testnet-magic ${NETWORK_ID} --socket-path /devnet/runtime/node.socket
  fi
}

function ccli_() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@}
  else
      ${DOCKER_COMPOSE_CMD} exec -T cardano-node cardano-cli ${@}
  fi
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

# Seed all participants in a single transaction
function seedAllParticipants() {
    echo >&2 "Seeding all participants in ONE transaction"
    echo >&2 "-----------------------------------"
    
    # Create runtime directory if it doesn't exist (for local mode)
    if [[ -x ${CCLI_CMD} ]]; then
        mkdir -p ${DEVNET_DIR}/runtime
    fi

    # Determine faucet address
    FAUCET_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/cardano-node/faucet.vk --testnet-magic ${NETWORK_ID})
    echo >&2 "Faucet address: ${FAUCET_ADDR}"

    # Query available UTXOs
    echo >&2 "Querying faucet UTXOs..."
    UTXO_JSON=$(ccli conway query utxo --address ${FAUCET_ADDR} --out-file /dev/stdout)
    
    # Check UTXOs
    UTXO_COUNT=$(echo "$UTXO_JSON" | jq 'length')
    if [[ "$UTXO_COUNT" == "0" ]]; then
        echo >&2 "ERROR: No UTXOs found at faucet address!"
        return 1
    fi
    
    echo >&2 "Found ${UTXO_COUNT} UTXO(s) at faucet address"
    
    # first available UTXO
    FAUCET_TXIN=$(echo "$UTXO_JSON" | jq -r 'keys[0]')
    UTXO_VALUE=$(echo "$UTXO_JSON" | jq -r ".\"${FAUCET_TXIN}\".value.lovelace")
    
    echo >&2 "Selected UTXO: ${FAUCET_TXIN}"
    echo >&2 "UTXO Value: ${UTXO_VALUE} lovelace ($(echo "scale=2; ${UTXO_VALUE}/1000000" | bc) Ada)"

    # Get all participant addresses
    echo >&2 ""
    echo >&2 "Building addresses for all participants..."
    ALICE_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/credentials/alice.vk --testnet-magic ${NETWORK_ID})
    BOB_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/credentials/bob.vk --testnet-magic ${NETWORK_ID})
    CAROL_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/credentials/carol.vk --testnet-magic ${NETWORK_ID})
    ALICE_FUNDS_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/credentials/alice-funds.vk --testnet-magic ${NETWORK_ID})
    BOB_FUNDS_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/credentials/bob-funds.vk --testnet-magic ${NETWORK_ID})
    CAROL_FUNDS_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/credentials/carol-funds.vk --testnet-magic ${NETWORK_ID})

    echo >&2 "  - alice: ${ALICE_ADDR}"
    echo >&2 "  - bob: ${BOB_ADDR}"
    echo >&2 "  - carol: ${CAROL_ADDR}"
    echo >&2 "  - alice-funds: ${ALICE_FUNDS_ADDR}"
    echo >&2 "  - bob-funds: ${BOB_FUNDS_ADDR}"
    echo >&2 "  - carol-funds: ${CAROL_FUNDS_ADDR}"
    
    echo >&2 ""
    echo >&2 "Building transaction with all 6 outputs..."
    
    # # Build transaction with all outputs in one go
    ccli conway transaction build --cardano-mode \
        --change-address ${FAUCET_ADDR} \
        --tx-in ${FAUCET_TXIN} \
        --tx-out ${ALICE_ADDR}+30000000 \
        --tx-out ${BOB_ADDR}+30000000 \
        --tx-out ${CAROL_ADDR}+30000000 \
        --tx-out ${ALICE_FUNDS_ADDR}+100000000 \
        --tx-out ${BOB_FUNDS_ADDR}+50000000 \
        --tx-out ${CAROL_FUNDS_ADDR}+25000000 \
        --out-file ${DEVNET_DIR}/runtime/seed-all-participants.draft >&2
    
    echo >&2 "Signing transaction..."
    ccli_ conway transaction sign \
        --tx-body-file ${DEVNET_DIR}/runtime/seed-all-participants.draft \
        --signing-key-file ${DEVNET_DIR}/cardano-node/faucet.sk \
        --out-file ${DEVNET_DIR}/runtime/seed-all-participants.signed >&2
    
    echo >&2 "Submitting transaction..."
    echo >&2 "Pre-submission check: verifying UTXO still exists..."
    UTXO_CHECK=$(ccli conway query utxo --tx-in ${FAUCET_TXIN} --out-file /dev/stdout)
    echo >&2 "UTXO check result: ${UTXO_CHECK}"

    SEED_TXID=$(ccli_ conway transaction txid --tx-file ${DEVNET_DIR}/runtime/seed-all-participants.signed | jq -r '.txhash' | tr -d '\r')
    ccli conway transaction submit --tx-file ${DEVNET_DIR}/runtime/seed-all-participants.signed >&2

    # Wait for the transaction to be confirmed
    # SEED_TXIN="${SEED_TXID}"
    # echo -n >&2 "Waiting for transaction confirmation.."
    
    # while [[ "$(ccli query utxo --tx-in "${SEED_TXIN}" --out-file /dev/stdout | jq ".\"${SEED_TXIN}\"")" = "null" ]]; do
    #     sleep 1
    #     echo -n >&2 "."
    # done
    
    echo >&2 " Done!"
    echo >&2 ""
    echo >&2 "✓ Transaction successful!"
    echo >&2 "--------------------------------"
    echo >&2 "Transaction ID: ${SEED_TXID}"
    echo >&2 ""
    echo >&2 "Outputs created:"
    echo >&2 "  #0: alice         -> 30 Ada"
    echo >&2 "  #1: bob           -> 30 Ada"
    echo >&2 "  #2: carol         -> 30 Ada"
    echo >&2 "  #3: alice-funds   -> 100 Ada"
    echo >&2 "  #4: bob-funds     -> 50 Ada"
    echo >&2 "  #5: carol-funds   -> 25 Ada"
    echo >&2 ""
    echo >&2 "Signed transaction file: ${DEVNET_DIR}/runtime/seed-all-participants.signed"
}

function publishReferenceScripts() {
  echo >&2 "Publishing reference scripts..."
  hnode publish-scripts \
    --testnet-magic ${NETWORK_ID} \
    --node-socket ${DEVNET_DIR}/runtime/node.socket \
    --cardano-signing-key ${DEVNET_DIR}/cardano-node/faucet.sk
}

${DOCKER_COMPOSE_CMD} exec cardano-node chown -R $(id -u):$(id -g) /devnet/runtime
function queryPParams() {
  echo >&2 "Query Protocol parameters"
  if [[ -x ${CCLI_CMD} ]]; then
     ccli query protocol-parameters --socket-path ${DEVNET_DIR}/runtime/node.socket  --out-file /dev/stdout \
      | jq ".txFeeFixed = 0 | .txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0" > ${SCRIPT_DIR}/runtime/protocol-parameters.json
   else
     ${DOCKER_COMPOSE_CMD} exec -T cardano-node cardano-cli query protocol-parameters --testnet-magic ${NETWORK_ID} --socket-path ${DEVNET_DIR}/runtime/node.socket --out-file /dev/stdout \
      | jq ".txFeeFixed = 0 | .txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0" > ${SCRIPT_DIR}/runtime/protocol-parameters.json
  fi
  echo >&2 "Saved in protocol-parameters.json"
}

# Main execution
echo >&2 "Fueling up all Hydra participants..."
seedAllParticipants

queryPParams
echo "HYDRA_SCRIPTS_TX_ID=$(publishReferenceScripts)" > .env
echo >&2 "Environment variable stored in '.env'"
echo >&2 -e "\n\t$(cat .env)\n"


