// @ts-nocheck
import {
  Address,
  AssetName,
  BaseAddress,
  BigNum,
  Ed25519KeyHash,
  Ed25519KeyHashes,
  EnterpriseAddress,
  hash_auxiliary_data,
  MultiAsset,
  NetworkId,
  PointerAddress,
  ScriptDataHash,
  ScriptHash,
  ScriptHashes,
  StakeCredential,
  Transaction,
  TransactionBody,
  TransactionInputs,
  TransactionOutput,
  TransactionUnspentOutput,
  TransactionWitnessSet,
  Value,
  Vkeywitnesses,
} from "@emurgo/cardano-serialization-lib-asmjs";

import type { CIP30Instace } from "@/types";
import { Buffer } from "buffer";

export async function signAndSubmit(provider: CIP30Instace, _tx: string) {
  let tx;
  try {
    const txArray = Uint8Array.from(Buffer.from(_tx, "hex"));
    tx = Transaction.from_bytes(txArray);
    const _txBody = tx.body();
    const txBody = TransactionBody.new_tx_body(
      _txBody.inputs(),
      _txBody.outputs(),
      _txBody.fee()
    );
    if (_txBody.mint()) {
      txBody.set_mint(_txBody.mint());
    }
    if (tx.auxiliary_data()) {
      txBody.set_auxiliary_data_hash(hash_auxiliary_data(tx.auxiliary_data()));
    }
    if (_txBody.collateral()) txBody.set_collateral(_txBody.collateral());
    if (_txBody.mint()) txBody.set_mint(_txBody.mint());
    if (_txBody.required_signers()) {
      txBody.set_required_signers(_txBody.required_signers());
    }
    if (_txBody.ttl_bignum()) {
      txBody.set_ttl(_txBody.ttl_bignum());
    }
    if (_txBody.validity_start_interval_bignum())
      txBody.set_validity_start_interval_bignum(
        _txBody.validity_start_interval_bignum()
      );
    if (_txBody.network_id && _txBody.network_id()) {
      txBody.set_network_id(_txBody.network_id());
    }
    if (_txBody.reference_inputs()) {
      txBody.set_reference_inputs(_txBody.reference_inputs());
    }
    if (_txBody.script_data_hash()) {
      txBody.set_script_data_hash(_txBody.script_data_hash());
    }
    if (_txBody.collateral_return()) {
      txBody.set_collateral_return(_txBody.collateral_return());
    }
    if (_txBody.total_collateral()) {
      txBody.set_total_collateral(_txBody.total_collateral());
    }

    // @ts-ignore
  } catch (e) {
    throw new Error("Invalid transaction string :" + e.message);
  }

  let txString = Buffer.from(tx.to_bytes()).toString("hex");

  const witnesesRaw = await provider.signTx(txString, true);
  const newWitnesses = TransactionWitnessSet.from_bytes(
    Buffer.from(witnesesRaw, "hex")
  );
  const newWitnessSet = TransactionWitnessSet.new();
  if (tx.witness_set().plutus_data())
    newWitnessSet.set_plutus_data(tx.witness_set().plutus_data());
  if (tx.witness_set().plutus_scripts())
    newWitnessSet.set_plutus_scripts(tx.witness_set().plutus_scripts());
  if (tx.witness_set().redeemers())
    newWitnessSet.set_redeemers(tx.witness_set().redeemers());
  if (tx.witness_set().native_scripts())
    newWitnessSet.set_native_scripts(tx.witness_set().native_scripts());
  // add the new witness.
  if (tx.witness_set().vkeys()) {
    const newVkeySet = Vkeywitnesses.new();

    for (let i = 0; i < tx.witness_set().vkeys().len(); i++) {
      newVkeySet.add(tx.witness_set().vkeys().get(i));
    }
    for (let i = 0; i < newWitnesses.vkeys().len(); i++) {
      newVkeySet.add(newWitnesses.vkeys().get(i));
    }
    newWitnessSet.set_vkeys(newVkeySet);
  } else {
    newWitnessSet.set_vkeys(newWitnesses.vkeys());
  }
  tx = Transaction.new(tx.body(), newWitnessSet, tx.auxiliary_data());
  const signedTxString = Buffer.from(tx.to_bytes()).toString("hex");
  // @ts-ignore
  console.log({
    additionWitnessSet: witnesesRaw,
    finalTx: signedTxString,
  });
  await provider.submitTx(signedTxString);
  return "Signed Tx: " + signedTxString;
}
export async function callWithProvider(f, ...params) {
  // @ts-ignore
  return window.cnftWithProvider((p) => f(p, ...params));
}
export async function withWalletProvider(f) {
  // @ts-ignore
  return window.cnftWithProvider((ins) =>
    f(ins).then(() => {
      // document.getElementById('wallet-overlay').classList.add("display-none")
    })
  );
}

function intDiv(a, b) {
  return Math.floor(a / b);
}

export function test() {
  console.log("testing");
}

export function listProviders() {
  // @ts-ignore
  const pluginMap = new Map();

  // @ts-ignore
  Object.keys(window.cardano).forEach((x) => {
    // @ts-ignore
    const plugin = window.cardano[x];
    if (plugin.enable && plugin.name) {
      pluginMap.set(plugin.name, plugin);
    }
  });
  const providers = Array.from(pluginMap.values());
  console.log("Provides", providers);
  return providers.filter((x) => x.name != "yoroi");
}

export async function getKeyHashOfAddressFromKuber(
  kuberUrl: string,
  address: string
) {
  return fetch(
    // eslint-disable-next-line max-len
    `${kuberUrl}/api/v1/keyhash`,
    {
      mode: "cors",
      method: "POST",
      body: JSON.stringify(address),
      headers: new Headers({ "content-type": "application/json" }),
    }
  )
    .catch((e) => {
      console.error(`${kuberUrl}/api/v1/tx`, e);
      throw Error(`Kubær API call : ` + e.message);
    })
    .then((res) => {
      if (res.status === 200) {
        return res.json().then((json) => {
          console.log(json);
          return json;
        });
      } else {
        return res.text().then((txt) => {
          let json: any;
          try {
            json = JSON.parse(txt);
          } catch (e) {
            return Promise.reject(
              Error(`KubærApi [Status ${res.status}] : ${txt}`)
            );
          }
          if (json) {
            return Promise.reject(
              Error(
                `KubærApi [Status ${res.status}] : ${
                  json.message ? json.message : txt
                }`
              )
            );
          } else {
            return Promise.reject(
              Error(`KubærApi [Status ${res.status}] : ${txt}`)
            );
          }
        });
      }
    });
}

export async function getPolicyIdOfScriptFromKuber(
  kuberUrl: string,
  scriptJsonStr: string
) {
  let scriptJson = JSON.parse(scriptJsonStr);
  return fetch(
    // eslint-disable-next-line max-len
    `${kuberUrl}/api/v1/scriptPolicy`,
    {
      mode: "cors",
      method: "POST",
      body: JSON.stringify(scriptJson),
      headers: new Headers({ "content-type": "application/json" }),
    }
  )
    .catch((e) => {
      console.error(`${kuberUrl}/api/v1/scriptPolicy`, e);
      throw Error(`Kubær API call : ` + e.message);
    })
    .then((res) => {
      if (res.status === 200) {
        return res.text().then((json) => {
          console.log(json);
          return json;
        });
      } else {
        return res.text().then((txt) => {
          let json: any;
          try {
            json = JSON.parse(txt);
          } catch (e) {
            return Promise.reject(
              Error(`KubærApi [Status ${res.status}] : ${txt}`)
            );
          }
          if (json) {
            return Promise.reject(
              Error(
                `KubærApi [Status ${res.status}] : ${
                  json.message ? json.message : txt
                }`
              )
            );
          } else {
            return Promise.reject(
              Error(`KubærApi [Status ${res.status}] : ${txt}`)
            );
          }
        });
      }
    });
}

export async function callKuberAndSubmit(
  provider: CIP30Instace,
  kuberUrl: string,
  data: string
) {
  const outputs = [];
  const kuberUrlByNetwork = "https://preview.kuber.sireto.dev";
  //  kuberUrl
  //   ? kuberUrl
  //   : import.meta.env.VITE_API_URL != undefined
  //   ? import.meta.env.VITE_API_URL
  //   : (await provider.getNetworkId()) == 0
  //   ? "https://preprod.cnftregistry.io/kuber"
  //   : "https://cnftregistry.io/kuber";

  var res = await fetch(
    // eslint-disable-next-line max-len
    `${kuberUrlByNetwork}/api/v1/tx`,
    {
      mode: "cors",
      method: "POST",
      body: data,
      headers: new Headers({ "content-type": "application/json" }),
    }
  ).catch((e) => {
    outputs.push(`${kuberUrlByNetwork}/api/v1/tx`);
    console.error(`${kuberUrlByNetwork}/api/v1/tx`, e);
    throw Error(`Kubær API call : ` + e.message);
  });

  if (res.status === 200) {
    var json = await res.json();
    console.log(json);
    outputs.push("Raw Txn: " + JSON.stringify(json));
    ("");
    const signedTx = await signAndSubmit(provider, json.tx).catch((e) => {
      if (e.info && e.code) {
        console.log(e);
        outputs.push(`Code : ${e.code} :: \n ${e.info}`);
        throw Error(`Code : ${e.code} :: \n ${e.info}`);
      } else throw e;
    });
    outputs.push(signedTx);
  } else {
    var txt = await res.text();

    let json: any;
    try {
      json = JSON.parse(txt);
    } catch (e) {
      outputs.push(`KubærApi [Status ${res.status}] : ${txt}`);
      return Promise.reject(Error(`KubærApi [Status ${res.status}] : ${txt}`));
    }
    if (json) {
      outputs.push(
        `KubærApi [Status ${res.status}] : ${json.message ? json.message : txt}`
      );

      return Promise.reject(
        Error(
          `KubærApi [Status ${res.status}] : ${
            json.message ? json.message : txt
          }`
        )
      );
    } else {
      outputs.push(`KubærApi [Status ${res.status}] : ${txt}`);
      return Promise.reject(Error(`KubærApi [Status ${res.status}] : ${txt}`));
    }
  }
  return outputs;
}

export function showWalletError(msg, time) {
  // @ts-ignore
  window.cnftError(msg, time);
}

export async function walletValue(provider): Promise<any> {
  const utxos: TransactionUnspentOutput[] = (await provider.getUtxos()).map(
    (u) => TransactionUnspentOutput.from_bytes(Buffer.from(u, "hex"))
  );
  const assets: Map<ScriptHash, Map<AssetName, BigNum>> = new Map();
  let adaVal = BigNum.zero();

  utxos.forEach((utxo) => {
    const value: Value = utxo.output().amount();
    adaVal = adaVal.checked_add(value.coin());
    if (value.multiasset()) {
      const multiAssets: ScriptHashes = value.multiasset().keys();
      for (let j = 0; j < multiAssets.len(); j++) {
        const policy: ScriptHash = multiAssets.get(j);
        const policyAssets = value.multiasset().get(policy);
        const assetNames = policyAssets.keys();
        let assetNameMap;
        assets.get(policy);
        if (!assetNameMap) {
          assets.set(policy, (assetNameMap = new Map()));
        }

        for (let k = 0; k < assetNames.len(); k++) {
          const policyAsset: AssetName = assetNames.get(k);
          let quantity = policyAssets.get(policyAsset);
          const oldQuantity: BigNum = assetNameMap.get(policyAsset);
          if (oldQuantity) {
            quantity = oldQuantity.checked_add(quantity);
          }
          assetNameMap.set(policyAsset, quantity);
        }
      }
    }
  });
  const assetObj = {};
  assets.forEach((k: Map<AssetName, BigNum>, v: ScriptHash) => {
    // eslint-disable-next-line no-multi-assign
    const policy = (assetObj[Buffer.from(v.to_bytes()).toString("hex")] = {});
    k.forEach((q: BigNum, a: AssetName) => {
      const assetName = Buffer.from(a.name());
      try {
        policy[assetName.toString("utf-8")] = BigInt(q.to_str());
      } catch (e) {
        policy[`0x${assetName.toString("hex")}`] = BigInt(q.to_str());
      }
    });
  });
  return {
    lovelace: adaVal,
    multiassets: assetObj,
  };
}
