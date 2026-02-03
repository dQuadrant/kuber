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

# Detect OS
OS_TYPE="$(uname)"

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


function waitForTx() {
    local TXID=$1
    local TIMEOUT=${2:-60} 
    local ELAPSED=0
    
    echo >&2 "Waiting for transaction ${TXID} to be confirmed..."
    
    while [[ $ELAPSED -lt $TIMEOUT ]]; do
        TX_CHECK=$(ccli conway query utxo --tx-in "${TXID}#0" --out-file /dev/stdout 2>/dev/null || echo "{}")
        
        if [[ $(echo "$TX_CHECK" | jq 'length') -gt 0 ]]; then
            echo >&2 ""
            echo >&2 "Transaction confirmed after ${ELAPSED} seconds"
            return 0
        fi
        
        echo >&2 -n "."
        sleep 2
        ELAPSED=$((ELAPSED + 2))
    done
    
    echo >&2 ""
    echo >&2 "WARNING: Transaction not confirmed after ${TIMEOUT} seconds"
    return 1
}


function verifyUTXO() {
    local ADDR=$1
    local NAME=$2
    local EXPECTED_AMOUNT=$3
    
    echo >&2 ""
    echo >&2 "Verifying ${NAME} (${ADDR})..."
    
    UTXO_RESULT=$(ccli conway query utxo --address ${ADDR} --out-file /dev/stdout 2>&1)
    UTXO_COUNT=$(echo "$UTXO_RESULT" | jq 'length' 2>/dev/null || echo "0")
    
    if [[ "$UTXO_COUNT" == "0" ]]; then
        echo >&2 "  NO UTXOs found for ${NAME}"
        echo >&2 "  Query result: ${UTXO_RESULT}"
        return 1
    else
        TOTAL_LOVELACE=$(echo "$UTXO_RESULT" | jq '[.[] | .value.lovelace] | add' 2>/dev/null || echo "0")
        TOTAL_ADA=$(echo "scale=6; ${TOTAL_LOVELACE}/1000000" | bc)
        echo >&2 "  ✓ Found ${UTXO_COUNT} UTXO(s) with total: ${TOTAL_ADA} Ada (${TOTAL_LOVELACE} lovelace)"
        return 0
    fi
}

# Seed all participants in a single transaction
function seedAllParticipants() {
    echo >&2 "Seeding....."

  
    FAUCET_ADDR=$(ccli_ conway address build --payment-verification-key-file ${DEVNET_DIR}/cardano-node/faucet.vk --testnet-magic ${NETWORK_ID})
    echo >&2 "Faucet address: ${FAUCET_ADDR}"

    echo >&2 "Querying faucet UTXOs..."
    UTXO_JSON=$(ccli conway query utxo --address ${FAUCET_ADDR} --out-file /dev/stdout)
    

    UTXO_COUNT=$(echo "$UTXO_JSON" | jq 'length')
    if [[ "$UTXO_COUNT" == "0" ]]; then
        echo >&2 "ERROR: No UTXOs found at faucet address!"
        echo >&2 "Make sure the devnet is running and synced."
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
    echo >&2 "Building transaction...."
    
    # Build transaction
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
    
    echo >&2 "Transaction built successfully"
    
    echo >&2 ""
    echo >&2 "Signing transaction..."
    ccli_ conway transaction sign \
        --tx-body-file ${DEVNET_DIR}/runtime/seed-all-participants.draft \
        --signing-key-file ${DEVNET_DIR}/cardano-node/faucet.sk \
        --out-file ${DEVNET_DIR}/runtime/seed-all-participants.signed >&2
    
    echo >&2 "Transaction signed successfully"
    
    echo >&2 ""
    SEED_TXID=$(ccli_ conway transaction txid --tx-file ${DEVNET_DIR}/runtime/seed-all-participants.signed | jq -r '.txhash' | tr -d '\r\n')
    
    echo >&2 ""
    echo >&2 "Submitting transaction..."
    ccli conway transaction submit --tx-file ${DEVNET_DIR}/runtime/seed-all-participants.signed >&2
    
    echo >&2 "Transaction submitted successfully"
    
   
    echo >&2 ""
    waitForTx "${SEED_TXID}" 120
    
    echo >&2 "Waiting 5 seconds for blockchain to sync..."
    sleep 5
    
    echo >&2 ""
    echo >&2 "Verifying funded addresses..."
    
    # Verify all addresses
    verifyUTXO "${ALICE_ADDR}" "alice" "30000000"
    verifyUTXO "${BOB_ADDR}" "bob" "30000000"
    verifyUTXO "${CAROL_ADDR}" "carol" "30000000"
    verifyUTXO "${ALICE_FUNDS_ADDR}" "alice-funds" "100000000"
    verifyUTXO "${BOB_FUNDS_ADDR}" "bob-funds" "50000000"
    verifyUTXO "${CAROL_FUNDS_ADDR}" "carol-funds" "25000000"
    
    echo >&2 ""
    echo >&2 ".......Seeding Complete!........"
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

#for macOS compatibility
if [[ "${OS_TYPE}" == "Linux" ]]; then
    ${DOCKER_COMPOSE_CMD} exec cardano-node chown -R $(id -u):$(id -g) /devnet/runtime 2>/dev/null || true
fi

function queryPParams() {
  echo >&2 ""
  echo >&2 "Querying Protocol parameters..."
  if [[ -x ${CCLI_CMD} ]]; then
     ccli query protocol-parameters --socket-path ${DEVNET_DIR}/runtime/node.socket  --out-file /dev/stdout \
      | jq ".txFeeFixed = 0 | .txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0" > ${SCRIPT_DIR}/runtime/protocol-parameters.json
   else
     ${DOCKER_COMPOSE_CMD} exec -T cardano-node cardano-cli query protocol-parameters --testnet-magic ${NETWORK_ID} --socket-path ${DEVNET_DIR}/runtime/node.socket --out-file /dev/stdout \
      | jq ".txFeeFixed = 0 | .txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0" > ${SCRIPT_DIR}/runtime/protocol-parameters.json
  fi
  echo >&2 "✓ Protocol parameters saved in runtime/protocol-parameters.json"
}

# Main execution
echo >&2 ""
echo >&2 "Seeding up all Hydra participants..."
echo >&2 ""

seedAllParticipants

echo >&2 ""
queryPParams

echo >&2 ""
echo "HYDRA_SCRIPTS_TX_ID=$(publishReferenceScripts)" > .env
echo >&2 "✓ Environment variable stored in '.env'"
echo >&2 -e "\n\t$(cat .env)\n"
echo >&2 ""
echo >&2 "All done!"
