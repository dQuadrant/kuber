<script setup lang="ts">
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
import jsonLogo from "@/src/assets/images/json_logo.png";

ace.require("ace/ext/language_tools");

ace.config.setModuleUrl("ace/mode/json_worker", workerJsonUrl);
</script>

<template>
  <vue-notification-list position="top-right"></vue-notification-list>
  <div class="flex flex-col items-center h-screen w-screen font-sans">
    <!-- top bar -->
    <div
      class="flex flex-col h-1/12 px-4 py-2 items-start w-full border border-borderColor"
    >
      <div class="flex w-full h-full justify-between items-center">
        <div class="flex items-center">
          <p class="text-primary">Create Tx with</p>
          <button
            v-if="language == LanguageEnums.Kuber"
            v-for="p in providers"
            :key="p.name"
            :class="
              provider.name == p.name
                ? 'ml-3 bg-transparent hover:bg-gray-100 text-primary font-semibold  py-0.5 px-1.5 border border-blue-500 hover:border-transparent rounded'
                : 'ml-3 bg-transparent hover:bg-gray-100 text-gray font-semibold  py-0.5 px-1.5 border border-gray-500 hover:border-transparent rounded'
            "
            @click="setProvider(p)"
          >
            <img
              style="display: inline; height: 1em; width: 1em"
              :src="p.icon"
            />
            <span class="ml-1"> {{ p.name }}</span>
          </button>
        </div>

        <!-- dropdown button -->
        <div class="dropdown">
          <button
            v-if="activeApi"
            :class="
              activeApi.border +
              ' dropdown-toggle border-2 font-sans font-semibold rounded-lg text-white text-[11pt] leading-tight uppercase hover:bg-gray-100  active:bg-gray-200 active:shadow-lg active:text-white transition duration-150 ease-in-out flex items-center justify-center whitespace-nowrap'
            "
            type="button"
            id="dropdownMenuButton1"
            data-bs-toggle="dropdown"
            aria-expanded="false"
          >
            <tag v-if="activeApi" :class="activeApi.text + ' py-1 pl-2'"
              >{{ activeApi.name }}
              <span class="pr-2">
                <v-icon name="md-keyboardarrowdown-round" />
              </span>
            </tag>
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
              class="hover:bg-gray-100 cursor-pointer"
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
        </div>
      </div>

      <!-- wallet utxos checkbox -->
      <div class="form-check mt-2 form-check text-sm">
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
    </div>

    <div class="flex w-full h-11/12">
      <!-- languages menu -->
      <div
        class="flex flex-col items-center space-y-4 w-1/20 py-4 bg-bgMenu h-full border border-borderColor"
      >
        <div class="flex justify-start space-x-3 h-12 w-full">
          <div
            v-if="language == LanguageEnums.Kuber"
            class="h-full w-1 bg-menuBar rounded-md shadow-sm"
          ></div>
          <div v-else class="h-full w-1 bg-bgMenu rounded-md"></div>
          <div
            @click="changeLanguage(LanguageEnums.Kuber)"
            :class="
              language == LanguageEnums.Kuber
                ? 'flex justify-center items-center w-14 rounded drop-shadow-sm border border-borderColor cursor-pointer bg-white hover:bg-white'
                : 'flex justify-center items-center w-14 rounded drop-shadow-sm border border-borderColor cursor-pointer hover:bg-white'
            "
          >
            <img src="@/assets/images/json_logo.png" />
          </div>
        </div>

        <div class="flex justify-start space-x-3 h-12 w-full">
          <div
            v-if="language == LanguageEnums.Haskell"
            class="h-full w-1 bg-menuBar rounded-md shadow-sm"
          ></div>
          <div v-else class="h-full w-1 bg-bgMenu rounded-md"></div>
          <div
            @click="changeLanguage(LanguageEnums.Haskell)"
            :class="
              language == LanguageEnums.Haskell
                ? 'flex justify-center items-center w-14 rounded drop-shadow-sm border border-borderColor cursor-pointer bg-white hover:bg-white'
                : 'flex justify-center items-center w-14 rounded drop-shadow-sm border border-borderColor cursor-pointer hover:bg-white'
            "
          >
            <img src="@/assets/images/haskell_logo.png" />
          </div>
        </div>
      </div>

      <!-- compiler screen -->
      <div
        class="flex flex-col w-13/20 h-full bg-bgCompiler border-y border-r border-borderColor"
      >
        <!-- file tabbar -->
        <div class="flex h-fileTabbar">
          <div
            class="flex font-sans font-medium items-center justify-center h-full w-32 bg-bgCompiler text-fileTextColor"
          >
            {{ language == LanguageEnums.Kuber ? "Kuber.json" : "Main.hs" }}
          </div>
          <div
            class="flex justify-end h-full w-full bg-bgFileTabBar border-x border-b border-borderColor py-3 px-4"
          >
            <div class="flex items-center h-full space-x-4">
              <div
                v-if="isCompiling"
                class="spinner-border animate-spin w-6 h-6 border-2 text-menuBar rounded-full"
              ></div>
              <div
                class="flex justify-center items-center bg-primary hover:bg-blue-600 text-white font-semibold text-sm w-16 h-full rounded-md shadow-sm cursor-pointer"
                @click="submitTx(provider)"
              >
                RUN
              </div>
            </div>
          </div>
        </div>

        <!-- code screen -->

        <!-- editor -->
        <div :class="editorHeight + ' grow pt-4'">
          <div id="monaco_editor" style="width: 100%; height: 100%"></div>
        </div>

        <!-- output terminal -->
        <div
          v-if="outputTerminalVisibility"
          class="flex flex-col w-full h-outputTerminal p-4 border-y border-borderColor"
        >
          <div class="flex justify-between">
            <div class="font-medium text-sm text-gray-500">OUTPUTS</div>
            <v-icon
              @click="showOutputTerminal(false)"
              class="cursor-pointer"
              name="io-close-outline"
              scale="1.2"
            />
          </div>
        </div>

        <!-- compiler tabbar -->
        <div
          class="flex w-full h-compilerTabbar border-t border-borderColor bg-bgUtilities"
        >
          <div
            @click="showOutputTerminal(!outputTerminalVisibility)"
            class="flex px-2 border-x border-borderColor text-sm text-gray-600 items-center cursor-pointer hover:bg-gray-100"
          >
            Outputs
          </div>
        </div>
      </div>

      <!-- utilities screen -->
      <div
        class="flex w-6/20 h-full bg-bgUtilities border-y border-borderColor"
      >
        <div class="flex w-19/20">
          <div class="flex flex-col w-full">
            <!-- <div
              class="flex justify-start items-center font-medium text-gray-600 text-sm h-16 w-full border-b border-borderColor bg-bgFileTabBar py-3 px-4"
            >
              UTILITIES
            </div> -->

            <div class="flex px-6 py-24">
              <!-- Address utitlity -->
              <div
                class="flex flex-col items-center"
                v-if="utility == UtilitiesEnums.Address"
              >
                <div class="mb-5 font-medium text-gray-600">Enter Address</div>
                <input
                  class="input border border-gray-300 focus:border focus:border-red-400"
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
                <div class="mt-5">
                  <button
                    @click="getKeyHash"
                    class="button-old hover:bg-green-600"
                  >
                    Get Key Hash
                  </button>
                </div>
              </div>

              <!-- Script Utilities -->
              <div
                class="flex flex-col items-center"
                v-if="utility == UtilitiesEnums.ScriptHash"
              >
                <div class="mb-5 font-medium text-gray-700">
                  Enter script json
                </div>
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
                          assets
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
                <div class="mt-6">
                  <button
                    @click="getScriptPolicy"
                    class="button-old hover:bg-green-600"
                  >
                    Get Policy Id
                  </button>
                </div>
              </div>

              <!-- Hex Utility -->
              <div
                class="flex flex-col w-full items-center"
                v-if="utility == UtilitiesEnums.Hex"
              >
                <div class="flex flex-col items-center w-full relative">
                  <div class="mb-5 text-gray-600 font-medium">Raw data</div>
                  <textarea
                    ref="rawData"
                    class="p-2 w-full min-h-[200pt] border border-gray-300 focus:ring-0"
                  />
                  <div class="mt-4 mb-4" v-if="result || errorMsg">
                    <div v-if="errorMsg" class="text-red-500">
                      {{ errorMsg }}
                    </div>
                    <div v-else>
                      <div class="text-gray-500 mb-1 text-left">Result</div>
                      <button
                        class="w-full hover:bg-slate-100 py-0.5"
                        @click="copyToClipboard(result)"
                      >
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
                  class="flex flex-shrink-0 flex-wrap items-center justify-start p-4 rounded-b-md"
                >
                  <button
                    @click="encodeHex"
                    class="button-old hover:bg-green-600"
                  >
                    Encode
                  </button>
                  <button
                    @click="decodeHex"
                    class="button-old hover:bg-green-600 ml-3"
                  >
                    Decode
                  </button>
                </div>
              </div>
            </div>
          </div>
        </div>
        <!-- utilites menu -->
        <div
          class="flex flex-col w-1/20 items-center font-semibold text-gray-600 text-sm justify-start bg-white h-full border border-borderColor"
        >
          <div
            @click="changeUtility(UtilitiesEnums.Address)"
            :class="
              utility == UtilitiesEnums.Address
                ? ' flex h-32 bg-bgSelectedUtility w-full border border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
                : 'flex h-32 bg-transparent w-full border border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
            "
          >
            <p class="rotate-90">Address</p>
          </div>

          <div
            @click="changeUtility(UtilitiesEnums.ScriptHash)"
            :class="
              utility == UtilitiesEnums.ScriptHash
                ? ' flex h-32 bg-bgSelectedUtility w-full border border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
                : 'flex h-32 bg-transparent w-full border border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
            "
          >
            <p class="rotate-90">ScriptHash</p>
          </div>

          <div
            @click="changeUtility(UtilitiesEnums.Hex)"
            :class="
              utility == UtilitiesEnums.Hex
                ? ' flex h-32 bg-bgSelectedUtility w-full border border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
                : 'flex h-32 bg-transparent w-full border border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
            "
          >
            <p class="rotate-90">Hex</p>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>
<script lang="ts">
import * as _notification from "@dafcoe/vue-notification";
import loader from "@monaco-editor/loader";
import { useToast } from "vue-toast-notification";
import {
  Address,
  BaseAddress,
  Ed25519KeyHash,
  EnterpriseAddress,
  PointerAddress,
} from "@emurgo/cardano-serialization-lib-asmjs";
import { SchemaKuber } from "./schemas";
import Description from "./descriptions";
import { LanguageEnums } from "@/models/enums/LanguageEnum";
import APIService from "@/services/api_service";
import { UtilitiesEnums } from "@/models/enums/UtilitiesEnum";

const notification = _notification.useNotificationStore();
var editor = null;
export default {
  mounted() {
    let counter = 8;
    const __this = this;
    this.editorInit();
    // APIService.compileCode();
    // this.haskellEditorInit();

    function refreshProvider() {
      __this.providers = listProviders();
      if (counter--) __this.timeout = setTimeout(refreshProvider, 1000);
      else __this.timeout = 0;
    }
    this.providers = listProviders();
    this.provider = this.providers.length != 0 ? this.providers[0] : null;
    this.timeout = setTimeout(refreshProvider, 1000);
  },
  data() {
    const providers: Array<CIP30Provider> = [];
    const provider: CIP30Provider = null;
    let defaultApi = {
      text: "text-[#60A5FA]",
      name: "Auto",
      border: "border-[#60A5FA]",
      display: "Mainnet/PreProd based on wallet NetworkId",
      url: undefined,
    };
    let result = {
      editorHeight: "h-editorStretch",
      outputTerminalVisibility: false,
      isCompiling: false,
      jsonLogo: jsonLogo,
      providers: providers,
      provider: provider,
      addSelections: true,
      utility: UtilitiesEnums.Address,
      language: LanguageEnums.Kuber,
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
          border: "border-blue-400",
          name: "Preview Testnet",
          display: "https://preview.cnftregistry.io/kuber",
          url: "https://preview.cnftregistry.io/kuber",
        },
        {
          text: "text-orange-400",
          border: "border-orange-400",
          name: "Preprod Testnet",
          display: "https://preprod.cnftregistry.io/kuber",
          url: "https://preprod.cnftregistry.io/kuber",
        },
        {
          text: "text-red-400",
          border: "border-red-400",
          name: "Mainnet",
          display: "https://cnftregistry.io/kuber",
          url: "https://cnftregistry.io/kuber",
        },
        {
          text: "text-gray-300",
          border: "border-gray-400",
          name: "Legacy Testnet",
          display: "https://testnet.cnftregistry.io/kuber",
          url: "https://testnet.cnftregistry.io/kuber",
        },
        {
          text: "text-gray-500",
          border: "border-blue-500",
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
    showOutputTerminal(visibility: boolean) {
      this.outputTerminalVisibility = visibility;
      if (visibility) {
        this.editorHeight = "h-editor";
      } else {
        this.editorHeight = "h-editorStretch";
      }
    },

    changeLanguage(language: LanguageEnums) {
      this.language = language;
      editor.setValue("");

      const model = editor.getModel();
      loader.init().then((monaco) => {
        monaco.editor.setModelLanguage(model, language);
      });
    },

    changeUtility(utility: UtilitiesEnums) {
      this.utility = utility;
    },

    setProvider(provider: CIP30Provider) {
      this.provider = provider;
    },

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
        const decoded = Buffer.from(val, "hex");
        if (decoded.toString("hex") === val) {
          let result = decoded.toString("utf-8");
          console.log("decoded", result);
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
      let addr = Address.from_bech32(this.address);
      let addrBase = BaseAddress.from_address(addr);
      let addrPointer = PointerAddress.from_address(addr);
      let addrEnterprise = EnterpriseAddress.from_address(addr);
      let keyHash: Ed25519KeyHash;
      if (addrBase) {
        console.log("hashKind", addrBase.payment_cred().kind);
        keyHash = addrBase.payment_cred().to_keyhash();
      } else if (addrPointer) {
        keyHash = addrPointer.payment_cred().to_keyhash();
      } else if (addrEnterprise) {
        keyHash = addrEnterprise.payment_cred().to_keyhash();
      }
      let keyHashHex = Buffer.from(keyHash.to_bytes()).toString("hex");

      this.keyHash = keyHashHex;

      // getKeyHashOfAddressFromKuber(this.activeApi.url, this.address)
      //   .catch((err) => alert(err))
      //   .then((res) => {
      //     this.keyHash = res.keyHash;
      //   });
    },
    getScriptPolicy() {
      // TODO do this with serialization library and not by calling api
      getPolicyIdOfScriptFromKuber(
        this.activeApi.url || this.apis.find((x) => x.name == "Mainnet").url,
        this.scriptJson
      )
        .catch((err) => alert(err))
        .then((res: string) => {
          this.policyId = res;
        });
    },
    submitTx(provider: CIP30Provider) {
      this.isCompiling = true;
      if (editor != null) {
        var editorContent = editor.getValue();
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
            if (
              request.collaterals &&
              typeof request.collaterals.push === "function"
            ) {
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
              this.isCompiling = false;
              return callKuberAndSubmit(
                instance,
                this.activeApi.url,
                JSON.stringify(request)
              );
            } else {
              this.isCompiling = false;
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
      }
    },

    editorInit() {
      // intializing monaco editor

      loader.init().then((monaco) => {
        var jsonCode = ["{}"].join("\n");
        var modelUri = monaco.Uri.parse("a://b/kuber.json");
        var model = monaco.editor.createModel(jsonCode, "json", modelUri);
        monaco.languages.register({ id: "haskell" });
        monaco.languages.json.jsonDefaults.setDiagnosticsOptions({
          validate: true,
          schemas: [
            {
              uri: "http://myserver/kuber-schema.json",
              fileMatch: [modelUri.toString()],
              schema: SchemaKuber,
            },
          ],
        });
        // registering hover provider
        monaco.languages.registerHoverProvider("json", {
          // @ts-ignore
          provideHover: function (model, position) {
            const wordDetails = model.getWordAtPosition(position);
            const description = Description.kuberDescription(wordDetails.word);
            return {
              range: new monaco.Range(
                position.lineNumber,
                wordDetails.startColumn,
                model.getLineCount(),
                model.getLineMaxColumn(model.getLineCount())
              ),
              contents: [
                { value: "**DESCRIPTION**" },
                {
                  value: description,
                },
              ],
            };
          },
        });

        const theme = {
          base: "vs",
          inherit: true,
          rules: [
            {
              token: "custom-info",
              background: "ffffff",
            },
            { token: "custom-error", foreground: "ee4444" },
            { token: "custom-notice", foreground: "1055af" },
            { token: "custom-date", foreground: "20aa20" },
          ],
          colors: {
            "editor.background": "#F5F5F5",
          },
        };

        // @ts-ignore
        monaco.editor.defineTheme("myTheme", theme);

        editor = monaco.editor.create(
          document.getElementById("monaco_editor"),
          {
            model: model,
            minimap: { enabled: false },
            theme: "myTheme",
            automaticLayout: true,
          }
        );
      });
    },
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
