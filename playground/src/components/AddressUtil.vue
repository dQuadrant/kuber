<script lang="ts" setup>
import {
  Address,
  StakeCredential,
  BaseAddress,
  PointerAddress,
  EnterpriseAddress,
  Ed25519KeyHash,
  ScriptHash,
} from "@emurgo/cardano-serialization-lib-asmjs";
import { Buffer } from "buffer";
import { useToast } from "vue-toast-notification";
enum KeyType {
  scriptHash = "Script Hash",
  pubKeyHash = "Key Hash",
  referencePointer = "Pointer"
}
enum Networks {
  Testnet = "Testnet",
  Mainnet = "Mainnet"
}
interface CredentialParseResult {
  error?: string,
  value?: StakeCredential
}
</script>
<template>
  <div class="w-full  ">
    <div class="w-full text-center font-bold text-xl border-b py-2 bg-slate-200 border-r-red-900 p">
      {{ composing?'Composing': 'Decomposing' }} Address
    </div>
    <div class="flex flex-col px-2 pt-4">
      <div v-if="!composing">
        <label>Address</label>
        <input class="addr-input w-full" :class="{ error: error.address }" type="text" v-model="addressInput" />
        <div class="pr-2 float-right text-red-500" v-if="error.address">{{ error.address }}</div>
      </div>
      <div v-else class="flex flex-col w-full items-start">
        <label for="addr-network-id">Network</label>

        <select name="addr-network-id" v-model="network" class="addr-input">
          <option>Testnet</option>
          <option>Mainnet</option>
        </select>
        <div class="w-full mt-4">
          <label for="payment-credential">Payment Credential</label>
          <div class="dropdown-container">
            <select id="payment-credential-type" v-model="paymentKey.type" class="dropdown-input dropdown-input-select">
              <option>Script Hash</option>
              <option>Key Hash</option>
            </select>
            <input id="payment-credential" placeholder="Payment Credential" class="dropdown-input grow"
              :class="{ error: error.paymentKey }" type="text" v-model="paymentKeyInput" />
          </div>
          <div class="pr-2 float-right text-red-500" v-if="error.paymentKey">{{ error.paymentKey }}</div>

        </div>
        <div class="w-full mt-4">
          <label for="stake-credential">Stake Credential</label>
          <div class="dropdown-container">
            <select id="stake-credential-type" v-model="stakeKey.type" class="dropdown-input dropdown-input-select">
              <option>Script Hash</option>
              <option>Key Hash</option>
            </select>
            <input id="stake-credential" placeholder="Stake Credential" class="dropdown-input grow"
              :class="{ error: error.stakeKey }" type="text" v-model="stakeKeyInput" />
          </div>
          <div class="pr-2 float-right text-red-500" v-if="error.stakeKey">{{ error.stakeKey }}</div>
        </div>
      </div>
      <!-- <hr class="border my-3" /> -->
      <div class=" mb-4 mt-8 text-center">
        <button @click="onPreformAction" class="button hover:bg-green-600">
          {{ composing?'Compose': 'Decompose' }}
        </button>

        <button @click="switchEdit"
          class="ml-3 rounded py-1 px-2 border-2 border-gray-300 hover:bg-gray-300 hover:border-r-gray-400 ">
          <v-icon name="bi-arrow-down-up"></v-icon>
        </button>
      </div>
      <div v-if="composing">
        <div class="mt-2" v-if="address && !error.stakeKey && !error.paymentKey">
          <label class="text-xl">Address</label>
          <div class="flex content-center py-2 select-none" @click="writeClipboard(address)">
            <div class="overflow-scroll pb-2 grow scroll-m-none mr-1 text-gray-700 font-mono ml-1">
              {{ address.substring(0, 20) }}...{{ address.substring(address.length - 20) }}
            </div>
            <v-icon name="fa-regular-clone" />
          </div>
        </div>
      </div>
      <div v-else-if="paymentKey.value">
        <label class="text-xl font-semibold">Network</label>
        <div class="ml-3"> {{ network }}</div>
        <hr class="border mb-8 mt-6 w-full" />
        <label class="text-xl">Payment Credential</label>
        <div class="flex content-center py-2 select-none" @click="writeClipboard(paymentKey.value)">
          <div class="whitespace-nowrap font-semibold text-md">{{
            paymentKey.type == KeyType.pubKeyHash ? 
            'Key Hash' : 'Script Hash'}}</div>
          <div class="overflow-scroll scrollbar-none grow scroll-m-none mr-1 text-gray-700 font-mono ml-5">{{
            paymentKey.value
          }}</div>
          <v-icon name="fa-regular-clone" />
        </div>
        <div v-if="stakeKey.value" class="mt-5">
          <hr class="border mb-8" />
          <label class="text-xl">Stake Credential</label>
          <div class="flex content-center py-2 select-none" @click="writeClipboard(stakeKey.value)">
            <div class="whitespace-nowrap font-semibold text-md">{{
              stakeKey.type == KeyType.pubKeyHash ? 
              'Key Hash' : 'ScriptHash'}}</div>
            <div class="overflow-scroll scrollbar-none grow scroll-m-none mr-1 text-gray-700 font-mono ml-5">{{
                stakeKey.value
            }}</div>
            <v-icon name="fa-regular-clone" />
          </div>

        </div>
      </div>

    </div>
  </div>
</template>
<script lang="ts">
export default {
  data() {
    return {
      address: "",
      stakeKey: {
        type: KeyType.pubKeyHash,
        value: ""
      },
      paymentKey: {
        type: KeyType.pubKeyHash,
        value: ""
      },
      network: Networks.Testnet,

      paymentKeyInput: "",
      stakeKeyInput: "",
      addressInput: "",
      composing: false,
      error: {
        address: "",
        paymentKey: "",
        stakeKey: ""

      }
    };
  },
  methods: {
    switchEdit() {
      if (this.composing) {
        if (this.address) {
          this.addressInput = this.address
        }
        this.error.address = ""
      } else if (this.paymentKey.value) {
        this.stakeKeyInput = this.stakeKey.value
        this.paymentKeyInput = this.paymentKey.value
        this.error.paymentKey = ""
        this.error.stakeKey = ""
      }
      this.composing = !this.composing;

    },
    writeClipboard(value){
        navigator.clipboard.writeText(value);
        useToast().success("Copied :" + value);
    },
    onPreformAction() {
      if (this.composing) {
        this.getAddressFromHashKeys(this.paymentKeyInput, this.stakeKeyInput)
      } else {
        this.getKeyHash()

      }
    },
    getKeyHash() {
      let addr: Address;
      if (this.addressInput.startsWith("addr")) {
        addr = this.tryorNull(() => Address.from_bech32(this.addressInput));
      } else if (this.addressInput.length == 58) {
        addr = this.tryorNull(() => Address.from_hex(this.addressInput));
      }
      if (!addr) {
        this.error.address = "Parse Error"
        return
      }
      this.network = addr.network_id() == 0 ? Networks.Testnet :Networks.Mainnet
      let addrBase = BaseAddress.from_address(addr);
      let addrPointer = PointerAddress.from_address(addr);
      let addrEnterprise = EnterpriseAddress.from_address(addr);
      const extractCredential = (credential: StakeCredential) => {
        if (credential.kind() == 0) {
          return {
            type: KeyType.pubKeyHash,
            value: Buffer.from(credential.to_keyhash().to_bytes()).toString('hex')
          }
        } else {
          return {
            type: KeyType.scriptHash,
            value: Buffer.from(credential.to_scripthash().to_bytes()).toString('hex')
          }
        }
      }
      if (addrBase) {
        this.paymentKey = extractCredential(addrBase.payment_cred())
        this.stakeKey = extractCredential(addrBase.stake_cred())
      } else if (addrPointer) {
        this.paymentKey = extractCredential(addrPointer.payment_cred())
      } else if (addrEnterprise) {
        this.paymentKey = extractCredential(addrEnterprise.payment_cred())
      } else {
        this.error.paymentKey = "Unknown Address Type"
        return
      }
      this.address = this.addressInput
      this.error.address = ""
    },
    tryorNull(f) {
      try {
        return f()
      } catch (e) {

      }
    },
    parseCredential(keyType: KeyType, credentialStr, errorOnEmpty = false): CredentialParseResult {
      if (credentialStr) {
        const credentialBytes: Buffer = Buffer.from(credentialStr, "hex")
        if ((credentialBytes.length * 2 != credentialStr.length) || !credentialBytes) {
          return { error: "Invalid Hex String" }
        }
        if (keyType == KeyType.pubKeyHash) {
          const keyHash = this.tryorNull(() => Ed25519KeyHash.from_bytes(credentialBytes))
          if (!keyHash) {
            return { error: "Invalid" }
          } else {
            return { value: StakeCredential.from_keyhash(keyHash) };
          }
        } else {
          const scriptHash = this.tryorNull(() => ScriptHash.from_bytes(credentialBytes))
          if (!scriptHash) {
            return { error: "Invalid" }
          } else {
            return { value: StakeCredential.from_scripthash(scriptHash) };
          }
        }
      } else {
        return {
          error: errorOnEmpty ? "Empty" : ""
        }
      }
    },
    getAddressFromHashKeys(paymentCredStr: string, stakeCredStr: string) {
      const network = this.network == Networks.Testnet ? 0 : 1;
      const paymentCred = this.parseCredential(this.paymentKey.type, paymentCredStr, true)
      const stakeCred = this.parseCredential(this.stakeKey.type, stakeCredStr)

      if (stakeCred.error || paymentCred.error) {
        this.error.paymentKey = paymentCred.error
        this.error.stakeKey = stakeCred.error
        this.address = ""
        return;
      }
      let addr: Address;
      if (stakeCred.value) {
        addr = BaseAddress.new(network, paymentCred.value, stakeCred.value).to_address();
      } else {
        addr = EnterpriseAddress.new(network, paymentCred.value).to_address()
      }
      this.address = addr.to_bech32()
      this.stakeKey.value=this.stakeKeyInput
      this.paymentKey.value=this.paymentKeyInput
      this.error.paymentKey = ""
      this.error.stakeKey = ""
    },
  },
};
</script>
<style scoped lang="postcss">
.addr-input {
  @apply focus:border-blue-400 border-y border-x-2 rounded p-1 py-2 text-base text-gray-700 bg-white bg-clip-padding focus:bg-gray-100;

}

.dropdown-input {
  @apply rounded-r p-1 py-2 text-base text-gray-700 bg-white bg-clip-padding focus:bg-gray-100 border-0 focus:outline-none;
}

.dropdown-input-select {
  @apply border-r focus:bg-gray-100 rounded-r-none rounded-l;
}

.dropdown-input.error {
  @apply border-red-500 border-b-2
}

.error {
  @apply border-b-red-500
}

label {
  @apply block font-semibold text-gray-600 mb-2
}

.dropdown-container:has(.dropdown-input:focus) {
  @apply border-blue-400 border-y border-x-2;
}

.dropdown-container {
  @apply flex w-full rounded shadow-sm border-y border-x-2 border-gray-400;
}

.button {
    border: none;
    color: #fff;
    background: green;
    appearance: none;
    font: inherit;
    padding: 0.5em;
    border-radius: 0.3em;
    cursor: pointer;
  }
  
</style>
