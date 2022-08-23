<script setup lang="ts">
import { VAceEditor } from "vue3-ace-editor";
import ace from "ace-builds";
import { Buffer } from "buffer";

import workerJsonUrl from "ace-builds/src-noconflict/worker-json?url";

import {
  callKuberAndSubmit,
  getKeyHashOfAddressFromKuber,
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

window.onclick = function (event) {
  console.log(event.target);
  if (event.target.id != "utility-dropdown-button")
    document.getElementById("utility-dropdown-list").classList.add("hidden");
};
</script>

<template>
  <div class="flex flex-col items-center self-center h-screen w-screen">
    <vue-notification-list position="top-right"></vue-notification-list>

    <div class="w-full px-2 pt-1">
      <div class="pb-3">
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
              <span class="dropdown relative">
                <button
                  class="dropdown-toggle px-3 py-2.5 bg-purple-500 text-white font-medium text-xs leading-tight uppercase rounded shadow-md hover:bg-purple-700 hover:shadow-lg focus:bg-purple-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-purple-800 active:shadow-lg active:text-white transition duration-150 ease-in-out flex items-center whitespace-nowrap"
                  type="button"
                  id="utility-dropdown-button"
                  data-bs-toggle="dropdown"
                  aria-expanded="false"
                  onclick="document.getElementById('utility-dropdown-list').classList.toggle('hidden')"
                >
                  Utilities
                  <svg
                    aria-hidden="true"
                    focusable="false"
                    data-prefix="fas"
                    data-icon="caret-down"
                    class="w-2 ml-2"
                    role="img"
                    xmlns="http://www.w3.org/2000/svg"
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
                  aria-labelledby="dropdownMenuButton2"
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
                  <li @click="displayHexEncodeModal">
                    <a
                      class="dropdown-item text-sm py-2 px-4 font-normal block w-full whitespace-nowrap bg-transparent text-gray-300 hover:bg-gray-700 hover:text-white focus:text-white focus:bg-gray-700"
                      href="#"
                      >Hex Encode/Decode</a
                    >
                  </li>
                </ul>
              </span>
            </span>
          </span>
          <transition name="fade" appear>
            <div class="modal" role="dialog" v-if="showKeyHashModal">
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
                  <button class="flex content-around" @click="performKeyHashCopy">
                    <div>{{ keyHash }}</div>
                    <div class="mt-1 float-right">
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
                <button @click="getKeyHash" class="button hover:bg-green-600">
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

          <transition name="pop" appear>
            <div class="modal" role="dialog" v-if="showPolicyModal">
              <div class="mb-2 text-gray-500">Enter script json</div>
              <textarea
                class="textarea border border-gray-300"
                :value="scriptJson"
                @input="onScriptJsonInput"
              />
              <div class="mt-4 mb-4" v-if="policyId != ''">
                <div class="text-gray-500 mb-1">Script policy id</div>
                <div>
                  <button class="flex" @click="performPolicyIdCopy">
                    <div>{{ policyId }}</div>
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
                <button @click="getScriptPolicy" class="button hover:bg-green-600">
                  Get Policy Id
                </button>
                <button
                  @click="showPolicyModal = false"
                  class="ml-4 border border-red-200 pt-2 pb-2 pl-4 pr-4 rounded text-gray-500 hover:bg-red-400 hover:text-white"
                >
                  Close
                </button>
              </div>
            </div>
          </transition>

          <transition name="fade" appear>
            <div class="modal" role="dialog" v-if="showHexEncoder">
              <div class="mb-2 text-gray-500">Raw Data</div>
              <textarea
                class="textarea border border-gray-300"
                ref="rawData"
              />
              <div class="mt-4 mb-4" v-if="result  || errorMsg">
                <div v-if="errorMsg" class="text-red-500">
                  {{errorMsg}}
                </div>
                <div v-else>
                  <div class="text-gray-500 mb-1 text-left">Result </div>
                  <button class="w-full" @click="copyToClipboard(result)">
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
              <div class="mt-3">
                <button @click="encodeHex" class="button hover:bg-green-600">
                  Enode Hex
                </button>
                <button @click="decodeHex" class="button ml-3 hover:bg-green-600">
                  Decode Hex
                </button>

                <button
                  @click="showHexEncoder = false"
                  class="float-right ml-4 border border-red-200 pt-2 pb-2 pl-4 pr-4 rounded text-gray-500 hover:bg-red-400 hover:text-white"
                >
                  Close
                </button>
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
    <!-- v-model="content" -->

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
import AppVue from "@/App.vue";
import { useToast } from "vue-toast-notification";

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

    return {
      providers: providers,
      addSelections: true,
      editor: null,
      interval: 0,
      timeout: 0,
      showKeyHashModal: false,
      showPolicyModal: false,
      showHexEncoder: false,
      address: "",
      keyHash: "",
      scriptJson: "",
      policyId: "",
      rawData: "",
      result: "",
      errorMsg: ""
    };
  },
  beforeUnmount() {
    this.interval && clearInterval(this.interval);
    this.timeout && clearTimeout(this.timeout);
  },
  methods: {
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
    encodeHex() {
      this.errorMsg="";
      const encoded = Buffer.from(this.$refs.rawData.value).toString("hex");
      this.result = encoded;
    },
    decodeHex() {
      this.errorMsg="";
      let val =this.$refs.rawData.value
      if(val){
        const decoded = Buffer.from(val, "hex").toString("utf-8");
        if(decoded && decoded.length * 2 == val.length){
          console.log("decoded",decoded)
          this.result=decoded
        }else{
          this.errorMsg="Invalid input"
        }
      }else{
        this.result = "";
      }
    },
    displayPolicyModal() {
      this.showPolicyModal = true;
      this.scriptJson = "";
      this.policyId = "";
    },
    displayHexEncodeModal() {
      this.showHexEncoder = true;
      this.rawData = "";
      this.result = "";
      this.errorMsg="";

    },
    getKeyHash() {
      getKeyHashOfAddressFromKuber(this.address)
        .catch((err) => alert(err))
        .then((res) => {
          this.keyHash = res.keyHash;
        });
    },
    getScriptPolicy() {
      getPolicyIdOfScriptFromKuber(this.scriptJson)
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
            return callKuberAndSubmit(instance, JSON.stringify(request));
          } else {
            return callKuberAndSubmit(instance, JSON.stringify(request));
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

.modal {
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
.modal h1 {
  margin: 0 0 1rem;
}

.modal-overlay {
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
</style>
