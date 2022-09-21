<script setup lang="ts">
import { VAceEditor } from "vue3-ace-editor";
import ace from "ace-builds";
import { Buffer } from "buffer";

import workerJsonUrl from "ace-builds/src-noconflict/worker-json?url";

import {
  callKuberAndSubmit,
  getPolicyIdOfScriptFromKuber,
  listProviders,
} from "@/scripts/wallet";
import type { CIP30Instace, CIP30Provider } from "@/types";

import "ace-builds/src-noconflict/mode-json";
import "ace-builds/src-noconflict/theme-chrome";
import "ace-builds/src-noconflict/ext-language_tools";

import suggestion from "../assets/suggestions.json";

ace.require("ace/ext/language_tools");

ace.config.setModuleUrl("ace/mode/json_worker", workerJsonUrl);
</script>

<template>
  <div class="flex flex-col items-center self-center h-screen w-screen">
    <vue-notification-list position="top-right"></vue-notification-list>
    <div class="w-full px-2 pt-1">
      <div class="pb-3">
        <span class="ml-0.5 mr-4 justify-center">
          <span>
            <span class="dropdown">
              <button
                class="dropdown-toggle text-white text-[11pt] leading-tight uppercase rounded hover:bg-gray-100 hover:shadow-lg active:bg-gray-200 active:shadow-lg active:text-white transition duration-150 ease-in-out flex items-center whitespace-nowrap"
                type="button"
                id="dropdownMenuButton1"
                data-bs-toggle="dropdown"
                aria-expanded="false"
              >
                <tag v-if="activeApi" :class="activeApi.text + ' py-1 px-4'">{{
                  activeApi.name
                }}</tag>
                <span
                  v-else
                  class="py-1 px-4 rounded-full text-blue-500 font-semibold flex align-center w-max cursor-pointer active:bg-gray-300 transition duration-300 ease"
                >
                </span>
              </button>
              <table
                class="left-0 dropdown-menu hidden min-w-max absolute bg-white text-base z-50 float-left py-2 text-left rounded-lg shadow-lg mt-1 m-0 bg-clip-padding border-2 border-blue-400 border-opacity-30"
                aria-labelledby="dropdownMenuButton1"
              >
                <tr
                  v-for="api in apis"
                  :key="api.display"
                  @click="handleApiSelected(api)"
                  class="hover:bg-gray-100"
                >
                  <td
                    :class="
                      api.text +
                      ' mr-4 border-b-2 dropdown-item text-sm py-2 px-4 font-normal bg-transparent text-center text-gray-700'
                    "
                  >
                    {{ api.name }}
                  </td>
                  <td
                    class="border-b-2 dropdown-item text-sm py-2 px-4 font-normal bg-transparent text-gray-700"
                  >
                    {{ api.display }}
                  </td>
                </tr>
              </table>
            </span>
          </span>
        </span>
        <span class="text-blue-800"> Create Tx with </span>
        <span>
          <button
            v-for="provider in providers"
            :key="provider.name"
            class="ml-3 bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-0.5 px-1.5 border border-blue-500 hover:border-transparent rounded"
            @click="submitTx(provider)"
          >
            <img style="display: inline; height: 1em; width: 1em" :src="provider.icon" />
            <span class="ml-1"> {{ provider.name }}</span>
          </button>
          <span class="float-right mr-2 mt-1">
            <span>
              <span class="inline-block relative">
                <button
                  class="dropdown-toggle px-3 py-2.5 bg-purple-500 text-white font-medium text-xs leading-tight uppercase rounded shadow-md hover:bg-purple-700 hover:shadow-lg focus:bg-purple-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-purple-800 active:shadow-lg active:text-white transition duration-150 ease-in-out flex items-center whitespace-nowrap"
                  type="button"
                  id="utility-dropdown-button"
                  data-bs-toggle="dropdown"
                  aria-expanded="false"
                >
                  <span>Utilities</span>
                  <svg
                    aria-hidden="true"
                    focusable="false"
                    data-prefix="fas"
                    data-icon="caret-down"
                    class="w-2 ml-2"
                    role="img"
                    xmlxns="http://www.w3.org/2000/svg"
                    viewBox="0 0 320 512"
                  >
                    <path
                      fill="currentColor"
                      d="M31.3 192h257.3c17.8 0 26.7 21.5 14.1 34.1L174.1 354.8c-7.8 7.8-20.5 7.8-28.3 0L17.2 226.1C4.6 213.5 13.5 192 31.3 192z"
                    ></path>
                  </svg>
                </button>
                <ul
                  id="utility-dropdown-list"
                  class="dropdown-menu min-w-max absolute right-0 hidden text-base z-50 float-right py-2 list-none text-left rounded-lg shadow-lg mt-1 m-0 bg-clip-padding border-none bg-gray-800"
                  aria-labelledby="utility-dropdown-button"
                >
                  <li @click="displayKeyHashModal">
                    <a
                      class="dropdown-item text-sm py-2 px-4 font-normal block w-full whitespace-nowrap bg-transparent text-gray-300 hover:bg-gray-700 hover:text-white focus:text-white focus:bg-gray-700 active:bg-blue-600"
                      href="#"
                      >Compute PubKey Hash</a
                    >
                  </li>
                  <li @click="displayPolicyModal">
                    <a
                      class="dropdown-item text-sm py-2 px-4 font-normal block w-full whitespace-nowrap bg-transparent text-gray-300 hover:bg-gray-700 hover:text-white focus:text-white focus:bg-gray-700"
                      href="#"
                      >Compute ScriptHash</a
                    >
                  </li>

                  <li>
                    <a
                      href="#"
                      class="dropdown-item text-sm py-2 px-4 font-normal block w-full whitespace-nowrap bg-transparent text-gray-300 hover:bg-gray-700 hover:text-white focus:text-white focus:bg-gray-700"
                      data-bs-toggle="modal"
                      data-bs-target="#hexEncoderModal"
                    >
                    Hex Encode/Decode
                    </a>
                  </li>
                </ul>
              </span>
            </span>
          </span>
          <transition name="fade" appear>
            <div
              class="modal-overlay-old"
              v-if="showKeyHashModal"
              @click="showKeyHashModal = false"
            ></div>
          </transition>
          <transition name="pop" appear>
            <div class="modal-old" role="dialog" v-if="showKeyHashModal">
              <div class="mb-2 text-gray-500">Enter Address</div>
              <input
                class="input border border-gray-300"
                type="text"
                :value="address"
                @input="onAddressInput"
              />
              <div class="mt-4 mb-4" v-if="keyHash != ''">
                <div class="text-gray-500 mb-1">Your keyhash</div>
                <div>
                  <button class="flex" @click="performKeyHashCopy">
                    <div>{{ keyHash }}</div>
                    <div class="mt-1">
                      <svg
                        xmlns="http://www.w3.org/2000/svg"
                        width="16"
                        height="16"
                        fill="currentColor"
                        class="bi bi-files"
                        viewBox="0 0 16 16"
                      >
                        <path
                          d="M13 0H6a2 2 0 0 0-2 2 2 2 0 0 0-2 2v10a2 2 0 0 0 2 2h7a2 2 0 0 0 2-2 2 2 0 0 0 2-2V2a2 2 0 0 0-2-2zm0 13V4a2 2 0 0 0-2-2H5a1 1 0 0 1 1-1h7a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1zM3 4a1 1 0 0 1 1-1h7a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1V4z"
                        />
                      </svg>
                    </div>
                  </button>
                </div>
              </div>
              <div class="mt-3">
                <button @click="getKeyHash" class="button-old hover:bg-green-600">
                  Get Key Hash
                </button>
                <button
                  @click="showKeyHashModal = false"
                  class="ml-4 border border-red-200 pt-2 pb-2 pl-4 pr-4 rounded text-gray-500 hover:bg-red-400 hover:text-white"
                >
                  Close
                </button>
              </div>
            </div>
          </transition>

          <transition name="fade" appear>
            <div
              class="modal-overlay-old"
              v-if="showPolicyModal"
              @click="showPolicyModal = false"
            ></div>
          </transition>
           <transition name="pop" appear>
            <div class="modal-old"
                 role="dialog"
                 v-if="showPolicyModal"
            >
                <div class="mb-2 text-gray-500">Enter script json</div>
                <textarea
                    class="textarea border border-gray-300"
                    :value="scriptJson"
                    @input="onScriptJsonInput"
                />
                <div class="mt-4 mb-4" v-if="policyId != ''" >
                  <div class="text-gray-500 mb-1">Script policy id</div>
                  <div>
                  <button class="flex" @click="performPolicyIdCopy">
                    <div>{{policyId}}</div>
                    <div class="mt-1"><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-files" viewBox="0 0 16 16">
                      <path d="M13 0H6a2 2 0 0 0-2 2 2 2 0 0 0-2 2v10a2 2 0 0 0 2 2h7a2 2 0 0 0 2-2 2 2 0 0 0 2-2V2a2 2 0 0 0-2-2zm0 13V4a2 2 0 0 0-2-2H5a1 1 0 0 1 1-1h7a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1zM3 4a1 1 0 0 1 1-1h7a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1V4z"/>
                    </svg></div>
                  </button>
                  </div>
                </div>
                <div class="mt-3">
                  <button @click="getScriptPolicy" class="button-old hover:bg-green-600">Get Policy Id</button>
                  <button @click="showPolicyModal = false" class="ml-4 border border-red-200 pt-2 pb-2 pl-4 pr-4 rounded text-gray-500 hover:bg-red-400 hover:text-white">Close</button>
                </div>
            </div>
          </transition>

          <div class="ml-2 mt-1 form-check form-check text-sm">
            <input
              class="form-check-input h-4 w-4 border border-gray-300 rounded-sm bg-white checked:bg-blue-600 checked:border-blue-600 focus:outline-none transition duration-200 mt-1 align-top bg-no-repeat bg-center bg-contain float-left mr-2 cursor-pointer"
              type="checkbox"
              id="inlineCheckbox1"
              v-model="addSelections"
            />
            <label
              class="form-check-label inline-block text-gray-500"
              for="inlineCheckbox1"
            >
              Add Wallet UTxOs in selection
            </label>
          </div>
        </span>
      </div>
    </div>
    <!-- Modal -->
    <div
      class="modal fade fixed top-0 left-0 hidden w-full h-full outline-none overflow-x-hidden overflow-y-auto"
      id="hexEncoderModal"
      tabindex="-1"
      aria-labelledby="exampleModalCenteredScrollable"
      aria-modal="true"
      role="dialog"
    >
      <div
        class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable relative w-auto pointer-events-none"
      >
        <div
          class="modal-content border-none shadow-lg relative flex flex-col w-full pointer-events-auto bg-white bg-clip-padding rounded-md outline-none text-current"
        >
          <div
            class="modal-header flex flex-shrink-0 items-center justify-between p-4 border-b border-gray-200 rounded-t-md"
          >
            <h5
              class="text-xl font-medium leading-normal text-gray-800"
              id="exampleModalCenteredScrollableLabel"
            >
              Hex Encoder/Decoder
            </h5>
            <button
              type="button"
              class="btn-close box-content w-4 h-4 p-1 text-black border-none rounded-none opacity-50 focus:shadow-none focus:outline-none focus:opacity-100 hover:text-black hover:opacity-75 hover:no-underline"
              data-bs-dismiss="modal"
              aria-label="Close"
            ></button>
          </div>

          <div class="modal-body relative p-4 pb-2">
            <div class="mb-2 text-gray-500">Raw data</div>
            <textarea
              ref="rawData"
              class="p-2 w-full min-h-[200pt] border border-gray-300"
            />
              <div class="mt-4 mb-4" v-if="result || errorMsg">
                <div v-if="errorMsg" class="text-red-500">
                  {{ errorMsg }}
                </div>
                <div v-else>
                  <div class="text-gray-500 mb-1 text-left">Result</div>
                  <button class="w-full hover:bg-slate-100 py-0.5" @click="copyToClipboard(result)">
                    <span class="">{{ result }}</span>
                    <span class="mt-1 pr-3 float-right">
                      <svg
                          xmlns="http://www.w3.org/2000/svg"
                          width="16"
                          height="16"
                          fill="currentColor"
                          class="bi bi-files"
                          viewBox="0 0 16 16"
                      >
                        <path
                            d="M13 0H6a2 2 0 0 0-2 2 2 2 0 0 0-2 2v10a2 2 0 0 0 2 2h7a2 2 0 0 0 2-2 2 2 0 0 0 2-2V2a2 2 0 0 0-2-2zm0 13V4a2 2 0 0 0-2-2H5a1 1 0 0 1 1-1h7a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1zM3 4a1 1 0 0 1 1-1h7a1 1 0 0 1 1 1v10a1 1 0 0 1-1 1H4a1 1 0 0 1-1-1V4z"
                        />
                      </svg>
                    </span>
                  </button>
                </div>
                </div>
              </div>
          <div
            class="modal-footer flex flex-shrink-0 flex-wrap items-center justify-start p-4 border-t border-gray-200 rounded-b-md"
          >
            <button @click="encodeHex" class="button-old hover:bg-green-600">
              Encode
            </button>
            <button @click="decodeHex" class="button-old hover:bg-green-600 ml-3">
              Decode
            </button>

            <button
              data-bs-dismiss="modal"
              class="border ml-auto mr-2 border-red-200 pt-2 pb-2 pl-4 pr-4 rounded text-gray-500 hover:bg-red-400 hover:text-white"
            >
              Close
            </button>
          </div>
        </div>
      </div>
    </div>
    <v-ace-editor
      value=""
      @init="editorInit"
      lang="json"
      theme="chrome"
      style="height: 100%; width: 100%"
    />
  </div>
</template>
<script lang="ts">
import * as _notification from "@dafcoe/vue-notification";
import { useToast } from "vue-toast-notification";
import { Address, BaseAddress, Ed25519KeyHash, EnterpriseAddress, PointerAddress } from '@emurgo/cardano-serialization-lib-asmjs';

const notification = _notification.useNotificationStore();
export default {
  mounted() {
    let counter = 8;
    const __this = this;

    function refreshProvider() {
      __this.providers = listProviders();
      if (counter--) __this.timeout = setTimeout(refreshProvider, 1000);
      else __this.timeout = 0;
    }
    this.providers = listProviders();
    this.timeout = setTimeout(refreshProvider, 1000);
  },
  data() {
    const providers: Array<CIP30Provider> = [];
    let defaultApi = {
      text: "text-gray-500",
      name: "Auto",
      display: "Mainnet/PreProd based on wallet NetworkId",
      url: undefined,
    };
    let result = {
      providers: providers,
      addSelections: true,
      editor: null,
      interval: 0,
      timeout: 0,
      showKeyHashModal: false,
      showPolicyModal: false,
      address: "",
      keyHash: "",
      scriptJson: "",
      policyId: "",
      rawData: "",
      result: "",
      errorMsg: "",
      activeApi: defaultApi,
      apis: [
        defaultApi,
        {
          text: "text-blue-400",
          name: "Preview Testnet",
          display: "https://preview.cnftregistry.io/kuber",
          url: "https://preview.cnftregistry.io/kuber",
        },
        {
          text: "text-orange-400",
          name: "Preprod Testnet",
          display: "https://preprod.cnftregistry.io/kuber",
          url: "https://preprod.cnftregistry.io/kuber",
        },
        {
          text: "text-red-400",
          name: "Mainnet",
          display: "https://cnftregistry.io/kuber",
          url: "https://cnftregistry.io/kuber",
        },
        {
          text: "text-gray-300",
          name: "Legacy Testnet",
          display: "https://testnet.cnftregistry.io/kuber",
          url: "https://testnet.cnftregistry.io/kuber",
        },
        {
          text: "text-gray-500",
          name: "Localhost",
          display: "http://localhost:8081",
          url: "http://localhost:8081",
        },
      ],
    };
    // result.activeApi=result.apis[0]
    return result;
  },
  beforeUnmount() {
    this.interval && clearInterval(this.interval);
    this.timeout && clearTimeout(this.timeout);
  },
  methods: {
    handleApiSelected(value) {
      this.activeApi = value;
      console.log("api selected", value);
    },
    onAddressInput(event) {
      this.address = event.target.value;
    },
    onScriptJsonInput(event) {
      this.scriptJson = event.target.value;
    },
    performKeyHashCopy() {
      useToast().success("Copied Key Hash");
      navigator.clipboard.writeText(this.keyHash);
    },
    copyToClipboard(data) {
      useToast().success("Copied :" + data);
      navigator.clipboard.writeText(data);
    },
    performPolicyIdCopy() {
      useToast().success("Copied Key Hash");
      navigator.clipboard.writeText(this.policyId);
    },
    displayKeyHashModal() {
      this.showKeyHashModal = true;
      this.address = "";
      this.keyHash = "";
    },
    displayPolicyModal() {
      this.showPolicyModal = true;
      this.scriptJson = "";
      this.policyId = "";
    },
    encodeHex() {
      this.errorMsg = "";
      //@ts-ignore
      const encoded = Buffer.from(this.$refs.rawData.value).toString("hex");
      this.result = encoded;
    },
    decodeHex() {
      this.errorMsg = "";
      //@ts-ignore
      let val = this.$refs.rawData.value;
      if (val) {
        const decoded = Buffer.from(val, "hex")
        if (decoded.toString("hex")  === val ) {
          let result= decoded.toString("utf-8")
          console.log("decoded",result );
          this.result = result;
        } else {
          this.errorMsg = "Invalid input";
        }
      } else {
        this.result = "";
      }
    },
    getKeyHash() {
      // TODO do this with serialization library and not by calling api
      let addr=Address.from_bech32(this.address)
      let addrBase= BaseAddress.from_address(addr)
      let addrPointer = PointerAddress.from_address(addr)
      let addrEnterprise = EnterpriseAddress.from_address(addr)
      let keyHash :Ed25519KeyHash
      if(addrBase){
        console.log("hashKind",addrBase.payment_cred().kind)
        keyHash=addrBase.payment_cred().to_keyhash()
      }else if (addrPointer){
        keyHash = addrPointer.payment_cred().to_keyhash();
      }else if (addrEnterprise){
        keyHash = addrEnterprise.payment_cred().to_keyhash();
      }
      let keyHashHex=Buffer.from(keyHash.to_bytes()).toString("hex")

      this.keyHash=keyHashHex

      // getKeyHashOfAddressFromKuber(this.activeApi.url, this.address)
      //   .catch((err) => alert(err))
      //   .then((res) => {
      //     this.keyHash = res.keyHash;
      //   });
    },
    getScriptPolicy() {
      // TODO do this with serialization library and not by calling api
      getPolicyIdOfScriptFromKuber(this.activeApi.url || (this.apis.find((x)=> x.name == 'Mainnet')).url, this.scriptJson)
        .catch((err) => alert(err))
        .then((res: string) => {
          this.policyId = res;
        });
    },
    submitTx(provider: CIP30Provider) {
      const editorContent = this.editor.getValue();
      this.save(editorContent);
      let request;
      try {
        request = JSON.parse(editorContent);
      } catch (e: any) {
        notification.setNotification({
          type: "alert",
          message: e.message,
        });
        return;
      }
      return provider
        .enable()
        .then(async (instance: CIP30Instace) => {
          const collateral = instance.getCollateral
            ? (await instance.getCollateral().catch(() => {})) || []
            : [];
          if (request.collaterals && typeof request.collaterals.push === "function") {
            collateral.forEach((x) => request.collaterals.push(x));
          } else if (collateral.length) {
            request.collaterals = collateral;
          }
          if (this.addSelections) {
            const availableUtxos = await instance.getUtxos();

            if (request.selections) {
              if (typeof request.selections.push === "function") {
                availableUtxos.forEach((v) => {
                  request.selections.push(v);
                });
              }
            } else {
              request.selections = availableUtxos;
            }
            return callKuberAndSubmit(
              instance,
              this.activeApi.url,
              JSON.stringify(request)
            );
          } else {
            return callKuberAndSubmit(
              instance,
              this.activeApi.url,
              JSON.stringify(request)
            );
          }
        })
        .catch((e: any) => {
          console.error("SubmitTx", e);
          notification.setNotification({
            type: "alert",
            message: e.message || "Oopsie, Nobody knows what happened",
          });
        });
    },
    editorInit(v: any) {
      // options for enabling autocompletion
      const options = {
        useWorker: true,
        autoScrollEditorIntoView: true,
        enableBasicAutocompletion: true,
        enableSnippets: true,
        enableLiveAutocompletion: true,
      };

      v.setOptions(options);

      const suggestionData = {
        getCompletions: function (editor, session, pos, prefix, callback) {
          if (prefix.length === 0) {
            callback(null, []);
            return;
          }
          callback(
            null,
            suggestion.map((suggestion) => suggestion)
          );
        },
      };

      // add the suggestion data to editor instance
      v.completers = [suggestionData];

      const session = v.getSession();
      session.setTabSize(2);
      session.setOptions({
        basicAutocompletion: true,
        useWorker: true,
      });
      session.setUseWrapMode(true);
      session.setValue(localStorage.getItem("editor.content") || "{\n\n}");
      this.interval = setInterval(() => {
        this.save(this.editor.getValue());
      }, 2000);
      this.editor = session;
      console.log(session);
    },
    save(v: string) {
      localStorage.setItem("editor.content", v);
    },
  },
  components: {
    VAceEditor,
  },
};
</script>
<style>
@import "../assets/base.css";
@import "tw-elements";
</style>

<style scoped>
.dropdown-content {
  display: none;
  position: absolute;
  background-color: #f1f1f1;
  min-width: 160px;
  overflow: auto;
  box-shadow: 0px 8px 16px 0px rgba(0, 0, 0, 0.2);
  z-index: 1;
}

.dropdown-content button {
  color: black;
  padding: 12px 16px;
  text-decoration: none;
  display: block;
}

.dropdown a:hover {
  background-color: #ddd;
}

.show {
  display: block;
}

.modal-new {
  position: absolute;
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
}

.modal-old {
  position: absolute;
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  margin: auto;
  text-align: center;
  width: fit-content;
  height: fit-content;
  padding: 2rem;
  border-radius: 1rem;
  box-shadow: 0 5px 5px rgba(0, 0, 0, 0.2);
  background: #fff;
  z-index: 999;
  transform: none;
}
.modal-old h1 {
  margin: 0 0 1rem;
}

.modal-overlay-old {
  content: "";
  position: absolute;
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  z-index: 998;
  background: #2c3e50;
  opacity: 0.6;
  cursor: pointer;
}

/* ---------------------------------- */
.fade-enter-active,
.fade-leave-active {
  transition: opacity 0.4s linear;
}

.fade-enter,
.fade-leave-to {
  opacity: 0;
}

.pop-enter-active,
.pop-leave-active {
  transition: transform 0.4s cubic-bezier(0.5, 0, 0.5, 1), opacity 0.4s linear;
}

.pop-enter,
.pop-leave-to {
  opacity: 0;
  transform: scale(0.3) translateY(-50%);
}

.dropdown {
  position: relative;
  display: inline-block;
}

.button-old {
  border: none;
  color: #fff;
  background: green;
  appearance: none;
  font: inherit;
  padding: 0.5em;
  border-radius: 0.3em;
  cursor: pointer;
}

.input {
  height: 50px;
  width: 500px;
  padding: 5px;
}

.textarea {
  height: 200px;
  width: 500px;
  padding: 5px;
}

button span {
  pointer-events: none;
}
button div {
  pointer-events: none;
}
button svg {
  pointer-events: none;
}
</style>
