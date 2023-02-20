<script setup lang="ts">
import { Buffer } from "buffer";
import {
  callKuber,
  getPolicyIdOfScriptFromKuber,
  listProviders,
  signTx,
  submitTx,
} from "kuber-client";
</script>

<template>
  <vue-notification-list position="top-right"></vue-notification-list>
  <div class="flex flex-col items-center h-screen w-screen font-sans">
    <!-- top bar -->
    <div
      class="flex flex-col h-1/12 px-4 py-1 2xl:text-base xl:text-sm lg:text-sm items-start w-full border border-borderColor"
    >
      <div class="flex w-full h-full justify-between items-center">
        <div class="flex items-center">
          <p
            class="text-primary font-semibold mr-8 2xl:text-lg xl:text-base lg:text-base"
          >
            Kuber Playground
          </p>
          <p v-if="language == LanguageEnums.Kuber" class="font-medium text-gray-600">
            Select wallet
          </p>
          <button
            v-if="language == LanguageEnums.Kuber"
            v-for="p in providers"
            :key="p.name"
            :class="
              provider.name === p.name
                ? 'flex items-center ml-3 bg-transparent  hover:bg-gray-100 text-primary font-semibold  py-0.5 px-1.5 border border-blue-500 hover:border-transparent rounded-md'
                : 'flex items-center ml-3 bg-transparent hover:bg-gray-100 text-gray-500 font-semibold  py-0.5 px-1.5 border border-gray-300 hover:border-transparent rounded-md'
            "
            @click="setProvider(p)"
          >
            <img style="display: inline; height: 1em; width: 1em" :src="p.icon" />
            <span class="ml-1"> {{ p.name }}</span>
            <v-icon
              v-if="provider.name == p.name"
              class="ml-2 cursor-pointer text-blue-500 2xl:w-5 2xl:h-5 xl:w-4 xl:h-4 lg:w-4 lg:h-4"
              name="bi-check-circle-fill"
            />
          </button>
        </div>
        <div class="flex items-center space-x-4 2xl:text-base xl:text-sm">
          <p class="font-medium text-gray-700">Network:</p>

          <!-- dropdown button -->
          <div class="dropdown 2xl:text-sm xl:text-xs lg:text-xs">
            <button
              v-if="activeApi"
              :class="
                activeApi.border +
                '   dropdown-toggle border-2 font-sans font-semibold rounded-lg text-white  leading-tight uppercase hover:bg-gray-100  active:bg-gray-200 active:shadow-lg active:text-white transition duration-150 ease-in-out flex items-center justify-center whitespace-nowrap'
              "
              type="button"
              id="dropdownMenuButton1"
              @click="handleNetworkDropDown()"
              aria-expanded="false"
            >
              <tag v-if="activeApi" :class="activeApi['text'] + ' py-1 pl-2'"
                >{{ activeApi["name"] }}
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
              v-if="networkDropdownVisibility"
              class="flex flex-col w-full right-0 min-w-max absolute bg-white text-base z-50 float-left pt-2 text-left rounded-lg shadow-lg mt-1 m-0 bg-clip-padding border-2 border-blue-400 border-opacity-30"
              aria-labelledby="dropdownMenuButton1"
            >
              <tr
                v-for="api in Object.values(apis)"
                :key="api['display']"
                @click="handleApiSelected(api)"
                class="border-b-2 hover:bg-gray-100 cursor-pointer 2xl:text-sm xl:text-xs lg:text-xs"
              >
                <td
                  :class="
                    api['text'] +
                    '  mr-4 dropdown-item  py-2 px-4 font-normal hover:bg-transparent bg-transparent text-center text-gray-700'
                  "
                >
                  {{ api["name"] }}
                </td>
                <td
                  class="dropdown-item py-2 px-4 font-normal hover:bg-transparent bg-transparent text-gray-700"
                >
                  {{ api["display"] }}
                </td>
              </tr>

              <button
                type="button"
                class="flex items-center cursor-pointer py-3 justify-center 2xl:text-sm xl:text-xs lg:text-xs px-4 font-semibold hover:bg-menuBar rounded-b-lg hover:text-white bg-transparent text-gray-700"
                data-bs-toggle="modal"
                data-bs-target="#staticBackdrop"
              >
                Edit/Add Network
                <span>
                  <v-icon class="ml-2" name="fa-edit" />
                </span>
              </button>
            </table>
          </div>
        </div>
      </div>

      <!-- wallet utxos checkbox -->
      <div
        v-if="language == LanguageEnums.Kuber"
        class="flex space-x-1 mt-[1px] items-center 2xl:text-sm xl:text-xs lg:text-xs"
      >
        <div class="flex space-x-2 items-center">
          <div class="form-check form-check">
            <input
              class="form-check-input 2xl:h-4 2xl:w-4 xl:w-3 xl:h-3 border border-gray-300 rounded-sm bg-white checked:bg-blue-600 checked:border-blue-600 focus:outline-none transition duration-200 mt-[1px] align-top bg-no-repeat bg-center bg-contain float-left mr-2 cursor-pointer"
              type="checkbox"
              id="inlineCheckbox1"
              v-model="addSelections"
            />
            <label
              class="form-check-label inline-block text-gray-500"
              for="inlineCheckbox1"
            >
              Add Wallet UTxOs in $.selections
            </label>
          </div>
          <div class="textover">
            <v-icon
              class="cursor-pointer text-gray-400 2xl:h-5 2xl:w-5 xl:w-4 xl:h-4 lg:w-4 lg:h-4"
              name="io-information-circle"
            /><span class="text"
              >Wallet utxos will be added to kuber selections field</span
            >
          </div>
        </div>
      </div>

      <div
        v-else
        class="2xl:mt-1 2xl:h-4 xl:mt-1 xl:h-4 lg:h-4 text-gray-500 2xl:text-sm xl:text-xs lg:text-xs"
      >
        Plutus Compiler
      </div>
    </div>

    <div class="flex w-full h-11/12">
      <!-- languages menu -->
      <div
        class="flex flex-col items-center space-y-4 w-1/20 py-4 bg-bgMenu h-full border border-borderColor"
      >
        <div
          class="flex justify-start 2xl:space-x-2 xl:space-x-2 lg:space-x-1 2xl:h-12 xl:h-11 lg:h-10 w-full"
        >
          <div
            v-if="language == LanguageEnums.Kuber"
            class="h-full w-1 bg-menuBar rounded-md shadow-sm"
          ></div>
          <div v-else class="h-full w-1 bg-bgMenu rounded-md"></div>
          <div
            @click="changeLanguage(LanguageEnums.Kuber)"
            :class="
              language == LanguageEnums.Kuber
                ? 'flex justify-center items-center 2xl:w-14 xl:w-12 lg:w-11  rounded drop-shadow-sm border border-borderColor cursor-pointer bg-white hover:bg-white'
                : 'flex justify-center items-center 2xl:w-14 xl:w-12 lg:w-11 rounded drop-shadow-sm border border-borderColor cursor-pointer hover:bg-white'
            "
          >
            <img src="@/assets/images/json_logo.png" />
          </div>
        </div>

        <div
          class="flex justify-start 2xl:space-x-2 xl:space-x-2 lg:space-x-1 2xl:h-12 xl:h-11 lg:h-10 w-full"
        >
          <div
            v-if="language == LanguageEnums.Haskell"
            class="h-full w-1 bg-menuBar rounded-md shadow-sm"
          ></div>
          <div v-else class="h-full w-1 bg-bgMenu rounded-md"></div>
          <div
            @click="changeLanguage(LanguageEnums.Haskell)"
            :class="
              language == LanguageEnums.Haskell
                ? 'flex justify-center items-center 2xl:w-14 xl:w-12 lg:w-11  lap rounded drop-shadow-sm border border-borderColor cursor-pointer bg-white hover:bg-white'
                : 'flex justify-center items-center 2xl:w-14 xl:w-12 lg:w-11  rounded drop-shadow-sm border border-borderColor cursor-pointer hover:bg-white'
            "
          >
            <img src="@/assets/images/haskell_logo.png" />
          </div>
        </div>
      </div>

      <!-- compiler screen -->
      <div
        :class="
          editorWidth +
          ' flex flex-col h-full 2xl:text-base xl:text-sm lg:text-sm bg-bgCompiler border-y border-r border-borderColor'
        "
      >
        <!-- kuber file tabbar -->
        <div
          v-if="language === LanguageEnums.Kuber"
          class="flex h-fileTabbar bg-bgFileTabBar disable-text-selection"
        >
          <div
            @click="changeKuberEditorTab(KuberTabEnums.KuberJson)"
            :class="
              kuberSelectedTab === KuberTabEnums.KuberJson
                ? 'flex  font-sans font-medium items-center justify-center h-full px-4 bg-bgCompiler text-fileTextColor cursor-pointer'
                : ' flex font-sans font-medium items-center justify-center h-full px-4 text-fileTextColor border-x border-b border-borderColor cursor-pointer'
            "
          >
            Kuber.json
          </div>
          <div
            @click="changeKuberEditorTab(KuberTabEnums.ExampleJson)"
            :class="
              kuberSelectedTab === KuberTabEnums.ExampleJson
                ? 'flex font-sans font-medium items-center justify-center h-full px-4 bg-bgCompiler text-fileTextColor cursor-pointer'
                : ' flex font-sans font-medium items-center justify-center h-full px-4 text-fileTextColor border-x border-b border-borderColor cursor-pointer'
            "
          >
            ExampleMetadata.json
          </div>
          <div
            @click="changeKuberEditorTab(KuberTabEnums.Readme)"
            :class="
              kuberSelectedTab === KuberTabEnums.Readme
                ? 'flex font-sans font-medium items-center justify-center h-full px-4 border-r border-borderColor bg-bgCompiler text-fileTextColor cursor-pointer'
                : ' flex font-sans font-medium items-center justify-center h-full px-4 text-fileTextColor border-x border-b border-borderColor cursor-pointer'
            "
          >
            README
          </div>

          <div
            class="flex justify-end h-full w-full bg-bgFileTabBar border-b border-borderColor xl:py-2 2xl:py-2 lg:py-1 px-4"
          >
            <div
              class="flex items-center h-full space-x-4 2xl:text-sm xl:text-xs lg:text-xs"
            >
              <div
                v-if="isCompiling"
                class="spinner-border animate-spin w-6 h-6 border-2 text-menuBar rounded-full"
              ></div>
              <div
                v-if="kuberSelectedTab === KuberTabEnums.KuberJson"
                class="flex justify-center items-center bg-primary hover:bg-blue-600 text-white font-semibold px-4 h-full rounded-md shadow-sm cursor-pointer"
                @click="constructSignAndSubmit(provider)"
              >
                RUN
              </div>
              <div
                v-else
                class="flex justify-center items-center bg-gray-300 text-white font-semibold px-4 h-full rounded-md shadow-sm cursor-not-allowed"
                @click=""
              >
                RUN
              </div>
            </div>
          </div>
        </div>

        <!-- haskell file tabbar  -->
        <div v-else class="flex h-fileTabbar bg-bgFileTabBar disable-text-selection">
          <div
            @click="changeHaskellEditorTab(HaskellTabEnums.ContractHs)"
            :class="
              haskellSelectedTab === HaskellTabEnums.ContractHs
                ? 'flex font-sans font-medium items-center justify-center h-full px-4 bg-bgCompiler text-fileTextColor cursor-pointer'
                : ' flex font-sans font-medium items-center justify-center h-full px-4 text-fileTextColor border-x border-b border-borderColor cursor-pointer'
            "
          >
            Contract.hs
          </div>
          <div
            @click="changeHaskellEditorTab(HaskellTabEnums.Readme)"
            :class="
              haskellSelectedTab === HaskellTabEnums.Readme
                ? 'flex font-sans font-medium items-center justify-center h-full px-4 border-r border-borderColor bg-bgCompiler text-fileTextColor cursor-pointer'
                : ' flex font-sans font-medium items-center justify-center h-full px-4 text-fileTextColor border-x border-b border-borderColor cursor-pointer'
            "
          >
            README
          </div>

          <div
            class="flex justify-end h-full w-full bg-bgFileTabBar border-b border-borderColor xl:py-2 2xl:py-2 lg:py-1 px-4"
          >
            <div
              class="flex items-center h-full space-x-4 2xl:text-sm xl:text-xs lg:text-xs"
            >
              <div
                v-if="isCompiling"
                class="spinner-border animate-spin w-6 h-6 border-2 text-menuBar rounded-full"
              ></div>
              <div
                v-if="haskellSelectedTab === HaskellTabEnums.ContractHs"
                class="flex justify-center items-center bg-primary hover:bg-blue-600 text-white font-semibold px-4 h-full rounded-md shadow-sm cursor-pointer"
                @click="compileCode()"
              >
                COMPILE
              </div>
              <div
                v-else
                class="flex justify-center items-center bg-gray-300 text-white font-semibold px-4 h-full rounded-md shadow-sm cursor-not-allowed"
                @click=""
              >
                COMPILE
              </div>
            </div>
          </div>
        </div>

        <!-- code screen -->

        <!-- editor -->
        <div :class="editorHeight + ' grow pt-4 text-xs'">
          <div id="monaco_editor" style="width: 100%; height: 100%" class="text-xs"></div>
        </div>

        <!-- output terminal -->

        <div
          v-if="outputTerminalVisibility"
          class="flex flex-col transition ease-in-out delay-4s w-full 2xl:text-sm xl:text-xs lg:text-xs h-outputTerminal px-4 pt-4 border-y border-borderColor"
        >
          <div class="flex justify-between h-1/6">
            <div class="font-medium text-gray-500">OUTPUTS</div>
            <v-icon
              @click="showOutputTerminal(false)"
              class="cursor-pointer"
              name="io-close-outline"
              scale="1.2"
            />
          </div>

          <!-- outputs-->
          <div
            id="haskell-output"
            v-if="language === LanguageEnums.Haskell"
            class="flex flex-col h-5/6 overflow-y-auto"
          >
            <pre v-for="output in haskellOutputs" class="text-gray-800 py-1">{{
              output
            }}</pre>
          </div>
          <div id="kuber-output" v-else class="flex flex-col h-5/6 overflow-y-auto">
            <pre v-for="output in kuberOutputs" class="text-gray-800 py-1">{{
              output
            }}</pre>
          </div>
        </div>

        <!-- compiler tabbar -->
        <div
          class="flex w-full h-compilerTabbar border-t border-borderColor 2xl:text-sm xl:text-xs lg:text-xs bg-bgUtilities"
        >
          <div
            @click="showOutputTerminal(!outputTerminalVisibility)"
            class="flex px-2 border-x border-borderColor text-gray-600 items-center cursor-pointer hover:bg-gray-100"
          >
            Outputs
          </div>
        </div>
      </div>

      <!-- utilities screen -->
      <div
        v-if="utilitiesVisibility"
        class="flex flex-col w-6/20 h-full bg-white border-y border-borderColor"
      >
        <div
          class="flex justify-between items-center font-medium text-gray-600 2xl:text-sm xl:text-xs lg:text-xs h-fileTabbar w-full border-b border-borderColor bg-white py-3 px-4"
        >
          <p>UTILITIES / {{ utility }}</p>
          <v-icon
            @click="showUtilities(false)"
            class="cursor-pointer"
            name="io-close-outline"
            scale="1.2"
          />
        </div>
        <div class="flex w-full 2xl:text-base xl:text-sm lg:text-sm">
          <!-- Address utitlity -->
          <AddressUtil v-if="utility == UtilitiesEnums.Address"></AddressUtil>
          <!-- Script Utilities -->
          <div
            class="flex w-full flex-col items-start px-4 py-8"
            v-if="utility == UtilitiesEnums.ScriptHash"
          >
            <div class="mb-5 font-semibold text-gray-500">Enter script json</div>
            <textarea
              class="textarea border border-gray-300 focus:border-gray-400"
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
              <button @click="getScriptPolicy" class="button-old hover:bg-green-600">
                Get Policy Id
              </button>
            </div>
          </div>

          <!-- Hex Utility -->
          <div
            class="flex flex-col w-full items-center px-4 py-8"
            v-if="utility == UtilitiesEnums.Hex"
          >
            <div class="flex flex-col items-start w-full relative">
              <div class="mb-5 text-gray-500 font-semibold">Raw data</div>
              <textarea
                :value="rawData"
                @input="onHexInput"
                class="p-2 textarea w-full min-h-[200pt] border border-gray-300 focus:border-gray-400"
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
            <div class="flex w-full items-start justify-start mt-4">
              <button @click="encodeHex" class="button-old hover:bg-green-600">
                Encode
              </button>
              <button @click="decodeHex" class="button-old hover:bg-green-600 ml-3">
                Decode
              </button>
            </div>
          </div>
        </div>
      </div>
      <!-- utilites menu -->
      <div
        class="flex flex-col w-utilitiesMenu items-center font-semibold text-gray-600 2xl:text-sm xl:text-sm lg:text-xs justify-start bg-white h-full border border-borderColor"
      >
        <div
          @click="changeUtility(UtilitiesEnums.Address)"
          :class="
            utility == UtilitiesEnums.Address
              ? ' flex h-32 bg-bgSelectedUtility w-full border-b border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
              : 'flex h-32 bg-transparent w-full  border-b border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
          "
        >
          <p class="rotate-90">Address</p>
        </div>

        <div
          @click="changeUtility(UtilitiesEnums.ScriptHash)"
          :class="
            utility == UtilitiesEnums.ScriptHash
              ? ' flex h-32 bg-bgSelectedUtility w-full border-y border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
              : 'flex h-32 bg-transparent w-full border-y border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
          "
        >
          <p class="rotate-90">ScriptHash</p>
        </div>

        <div
          @click="changeUtility(UtilitiesEnums.Hex)"
          :class="
            utility == UtilitiesEnums.Hex
              ? ' flex h-32 bg-bgSelectedUtility w-full border-y border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
              : 'flex h-32 bg-transparent w-full border-y border-borderColor items-center justify-center cursor-pointer hover:bg-gray-100'
          "
        >
          <p class="rotate-90">Hex</p>
        </div>
      </div>
      <!--Network Modal -->
      <div
        class="modal fade fixed top-0 left-0 hidden w-full h-full outline-none overflow-x-hidden overflow-y-auto"
        id="staticBackdrop"
        data-bs-backdrop="static"
        data-bs-keyboard="false"
        tabindex="-1"
        aria-labelledby="staticBackdropLabel"
        aria-hidden="true"
      >
        <div class="font-sans modal-dialog relative w-auto pointer-events-none">
          <div
            class="h-full modal-content border-none shadow-lg relative flex flex-col w-full pointer-events-auto bg-white bg-clip-padding rounded-xl outline-none text-current"
          >
            <div
              class="modal-header flex flex-shrink-0 text-sm text-gray-400 font-semibold items-center justify-between py-4 px-8 border-b border-gray-200 rounded-t-md"
            >
              <div>
                <span class="mr-2"><v-icon name="ri-settings-5-line" /></span>Edit/Add
                Networks
              </div>

              <v-icon
                data-bs-dismiss="modal"
                class="cursor-pointer text-gray-500"
                name="io-close-outline"
                scale="1.1"
              />
            </div>
            <div class="modal-body relative flex flex-col pt-6">
              <div class="flex text-sm font-semibold text-gray-400 space-x-4 px-8">
                <div
                  @click="changeNetworkTab(NetworkSettingEnums.EditNetwork)"
                  class="flex flex-col space-y-1 cursor-pointer"
                >
                  <div
                    :class="
                      networkSettingTab === NetworkSettingEnums.EditNetwork
                        ? 'text-gray-800'
                        : 'text-gray-400'
                    "
                  >
                    Edit Network
                  </div>
                  <div
                    v-if="networkSettingTab === NetworkSettingEnums.EditNetwork"
                    class="h-[2px] bg-menuBar rounded-full"
                  ></div>
                </div>

                <div
                  @click="changeNetworkTab(NetworkSettingEnums.AddNetwork)"
                  class="flex flex-col space-y-1 cursor-pointer"
                >
                  <div
                    :class="
                      networkSettingTab === NetworkSettingEnums.AddNetwork
                        ? 'text-gray-800'
                        : 'text-gray-400'
                    "
                  >
                    Add Network
                  </div>
                  <div
                    v-if="networkSettingTab === NetworkSettingEnums.AddNetwork"
                    class="h-[2px] bg-menuBar rounded-full"
                  ></div>
                </div>
              </div>

              <!-- Edit Network -->
              <div
                v-if="networkSettingTab === NetworkSettingEnums.EditNetwork"
                class="flex flex-col items-start mt-2 h-[295px] px-8 pb-4 overflow-y-auto"
              >
                <div
                  v-for="api in Object.values(apis)"
                  :key="api['display']"
                  class="flex w-full space-x-2 items-center justify-between pt-2"
                >
                  <div
                    :class="
                      activeApi['name'] === api['name']
                        ? api['text'] +
                          ' flex justify-start items-start w-1/3 mr-4  text-sm  font-semibold hover:bg-transparent  text-start '
                        : ' flex justify-start items-start w-1/3 mr-4  text-sm  font-semibold hover:bg-transparent  text-start text-gray-400'
                    "
                  >
                    {{ api["name"] }}
                    <span class="text-gray-400"
                      ><v-icon
                        v-if="api['name'] === 'Auto'"
                        class="ml-2"
                        name="hi-solid-lock-closed"
                    /></span>
                  </div>
                  <input
                    class="flex w-2/3 text-sm py-2 rounded-lg border border-borderColor px-4 font-normal hover:bg-transparent focus:border-blue-500 bg-transparent text-gray-700"
                    type="text"
                    :disabled="api['name'] === 'Auto'"
                    :onblur="saveEditing"
                    :value="
                      editingNetwork[api['name']] != null
                        ? editingNetwork[api['name']]
                        : api['display']
                    "
                    @input="
                      (event) => {
                        editNetwork(api['name'], event);
                      }
                    "
                  />
                  <span
                    v-if="
                      api['name'].toLowerCase() !== NetworkEnums.Auto &&
                      defaultNetworks.includes(api['name'].toLowerCase())
                    "
                    ><v-icon
                      @click="resetNetwork(api['name'])"
                      class="text-gray-400 hover:text-gray-600 cursor-pointer"
                      name="md-lockreset-round"
                  /></span>
                  <span v-else
                    ><v-icon class="text-white cursor-default" name="md-lockreset-round"
                  /></span>
                </div>
              </div>

              <!-- Add network -->
              <div
                v-if="networkSettingTab === NetworkSettingEnums.AddNetwork"
                class="flex flex-col items-start mt-2 h-[295px] px-8 pb-4 overflow-y-auto"
              >
                <div class="flex flex-col w-full space-y-2 pt-2">
                  <div
                    :class="' flex justify-start items-start w-1/3 mr-4  text-sm  font-semibold hover:bg-transparent  text-start text-gray-800'"
                  >
                    Network name
                  </div>
                  <input
                    id="name"
                    class="flex w-full text-sm py-2 rounded-lg border border-borderColor px-4 font-normal hover:bg-transparent bg-transparent text-gray-700 focus:border-blue-500"
                    type="text"
                    :value="newNetwork.name"
                    @input="handleAddNetwork"
                  />
                </div>

                <div class="flex flex-col w-full space-y-2 mt-4">
                  <div
                    :class="' flex justify-start items-start w-1/3 mr-4  text-sm  font-semibold hover:bg-transparent  text-start text-gray-800'"
                  >
                    URL
                  </div>
                  <input
                    id="url"
                    class="flex w-full text-sm py-2 rounded-lg border border-borderColor px-4 font-normal hover:bg-transparent bg-transparent text-gray-700 focus:border-blue-500"
                    type="text"
                    :value="newNetwork.url"
                    @input="handleAddNetwork"
                  />
                </div>
                <p
                  v-for="error in newNetwork.errors"
                  class="flex items-center text-red-500 text-xs pt-2"
                >
                  <span
                    ><v-icon class="mr-1" name="md-erroroutline-outlined" scale="0.9"
                  /></span>
                  {{ error }}
                </p>
                <div
                  class="flex justify-center items-center bg-primary hover:bg-blue-600 py-1 mt-5 text-white font-semibold text-sm w-16 rounded-md shadow-sm cursor-pointer"
                  @click="addNewNetwork()"
                >
                  Save
                </div>
              </div>
            </div>
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
  hash_transaction,
  PointerAddress,
} from "@emurgo/cardano-serialization-lib-asmjs";
import { SchemaKuber } from "./schemas";
import Description from "./descriptions";
import {
  LanguageEnums,
  KuberTabEnums,
  HaskellTabEnums,
} from "@/models/enums/LanguageEnum";
import APIService from "@/services/api_service";
import { UtilitiesEnums } from "@/models/enums/UtilitiesEnum";
import {
  DefaultComment,
  DefaultKuberCode,
  DefaultHaskellCode,
  HaskellCodes,
  KuberCodes,
  NetworkUrls,
  SimpleContractCode,
} from "@/models/constants";
import {
  AddNetworkErrorEnums,
  NetworkEnums,
  NetworkSettingEnums,
} from "@/models/enums/SettingEnum";
import AddressUtil from "./AddressUtil.vue";
import type { CIP30Instace, CIP30Provider } from "kuber-client/types";

const notification = _notification.useNotificationStore();

export default {
  components: { AddressUtil },
  editor: null,
  mounted() {
    let counter = 5;
    const __this = this;
    this.editorInit();

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
    const autoApi = {
      text: "text-[#60A5FA]",
      name: "Auto",
      border: "border-[#60A5FA]",
      display:
        import.meta.env.VITE_API_URL === undefined
          ? "Mainnet/PreProd based on wallet NetworkId"
          : "Same server's API backend",
      url: import.meta.env.VITE_API_URL,
    };
    const providers: Array<CIP30Provider> = [];
    const provider: CIP30Provider = null;
    let defaultApi = JSON.parse(localStorage.getItem("network")) || autoApi;
    const customNetworks = JSON.parse(localStorage.getItem("networks")) || {};

    let result = {
      editorHeight: "h-editorStretch",
      editorWidth: "w-editor",
      kuberSelectedTab: KuberTabEnums.KuberJson,
      haskellSelectedTab: HaskellTabEnums.ContractHs,
      editingNetwork: {},
      newNetwork: { name: "", url: "", errors: [] },
      haskellOutputs: [],
      networkSettingTab: NetworkSettingEnums.EditNetwork,
      kuberOutputs: [],
      outputTerminalVisibility: false,
      networkDropdownVisibility: false,
      utilitiesVisibility: true,
      customNetworks: customNetworks,
      isCompiling: false,
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
      stakeKeyHash: "",
      scriptJson: "",
      policyId: "",
      rawData: "",
      result: "",
      errorMsg: "",
      activeApi: defaultApi,
      defaultNetworks: [
        NetworkEnums.Auto,
        NetworkEnums.Localhost,
        NetworkEnums.Mainnet,
        NetworkEnums.PreprodTestnet,
        NetworkEnums.PreviewTestnet,
      ],
      apis: {
        auto: autoApi,
        "preview testnet": {
          text: "text-blue-400",
          border: "border-blue-400",
          name: "Preview Testnet",
          display:
            localStorage.getItem(NetworkEnums.PreviewTestnet) ||
            NetworkUrls[NetworkEnums.PreviewTestnet],
          url:
            localStorage.getItem(NetworkEnums.PreviewTestnet) ||
            NetworkUrls[NetworkEnums.PreviewTestnet],
        },
        "preprod testnet": {
          text: "text-orange-400",
          border: "border-orange-400",
          name: "Preprod Testnet",
          display:
            localStorage.getItem(NetworkEnums.PreprodTestnet) ||
            NetworkUrls[NetworkEnums.PreprodTestnet],
          url:
            localStorage.getItem(NetworkEnums.PreprodTestnet) ||
            NetworkUrls[NetworkEnums.PreprodTestnet],
        },
        mainnet: {
          text: "text-red-400",
          border: "border-red-400",
          name: "Mainnet",
          display:
            localStorage.getItem(NetworkEnums.Mainnet) ||
            NetworkUrls[NetworkEnums.Mainnet],
          url:
            localStorage.getItem(NetworkEnums.Mainnet) ||
            NetworkUrls[NetworkEnums.Mainnet],
        },
        localhost: {
          text: "text-gray-500",
          border: "border-blue-500",
          name: "Localhost",
          display:
            localStorage.getItem(NetworkEnums.Localhost) ||
            NetworkUrls[NetworkEnums.Localhost],
          url:
            localStorage.getItem(NetworkEnums.Localhost) ||
            NetworkUrls[NetworkEnums.Localhost],
        },
        ...customNetworks,
      },
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
    showUtilities(visibility: boolean) {
      this.utilitiesVisibility = visibility;
      if (!visibility) {
        this.editorWidth = "w-editorStretch";
        this.changeUtility(null);
      } else {
        this.editorWidth = "w-editor";
      }
    },

    changeNetworkTab(tab: NetworkSettingEnums) {
      this.networkSettingTab = tab;
    },

    changeKuberEditorTab(tab: KuberTabEnums) {
      this.saveCode(this.kuberSelectedTab);
      this.kuberSelectedTab = tab;
      this.loadKuberCode(tab);
    },

    changeHaskellEditorTab(tab: HaskellTabEnums) {
      this.saveCode(this.haskellSelectedTab);
      this.haskellSelectedTab = tab;
      this.loadHaskellCode(tab);
    },

    saveCode(id: string) {
      if (this.$options.editor != null) {
        var code = this.$options.editor.getValue();
        code = code.trim();
        localStorage.setItem(id, code);
      }
    },
    updateOutputScroll(id: string) {
      var element = document.getElementById(id);
      element.scrollTop = element.scrollHeight;
    },

    saveEditing() {
      const networkName = Object.keys(this.editingNetwork)[0];
      const value = this.editingNetwork[networkName];
      this.apis[networkName].display = value;
      this.apis[networkName].url = value;
      localStorage.setItem(networkName, value);
      // @ts-ignore
      if (!this.defaultNetworks.includes(networkName)) {
        this.customNetworks[networkName].display = value;
        this.customNetworks[networkName].url = value;
        localStorage.setItem("networks", JSON.stringify(this.customNetworks));
      }
      this.editingNetwork = {};
    },

    resetNetwork(networkName: string) {
      networkName = networkName.toLowerCase();
      localStorage.removeItem(networkName);
      this.apis[networkName].display = NetworkUrls[networkName];
      this.apis[networkName].url = NetworkUrls[networkName];
    },

    editNetwork(name: string, event) {
      this.editingNetwork[name.toLowerCase()] = event.target.value;
    },

    handleAddNetwork(event) {
      console.log(event);
      const id = event.target.id;
      if (id === "name") {
        const value = event.target.value;
        this.newNetwork.name = value;

        this.validateAddNetwork(id, value);
      } else {
        const value = event.target.value;
        this.newNetwork.url = event.target.value;
        this.validateAddNetwork(id, value);
      }
    },
    validateAddNetwork(id, value: string) {
      console.log(this.newNetwork.errors);
      if (id === "name") {
        const emptyNameIndex = this.newNetwork.errors.indexOf(
          AddNetworkErrorEnums.EmptyName
        );
        if (emptyNameIndex > -1) {
          this.newNetwork.errors.splice(emptyNameIndex, 1);
        }
        const duplicateNameIndex = this.newNetwork.errors.indexOf(
          AddNetworkErrorEnums.DuplicateName
        );
        if (this.apis[value.toLowerCase()]) {
          if (duplicateNameIndex === -1) {
            this.newNetwork.errors.push(AddNetworkErrorEnums.DuplicateName);
          }
        } else {
          if (duplicateNameIndex > -1) {
            this.newNetwork.errors.splice(duplicateNameIndex, 1);
          }
        }
      } else {
        const index = this.newNetwork.errors.indexOf(AddNetworkErrorEnums.EmptyUrl);
        if (index > -1) {
          this.newNetwork.errors.splice(index, 1);
        }
      }
    },

    addNewNetwork() {
      if (this.newNetwork.errors.length === 0) {
        if (this.newNetwork.name !== "") {
          const networkName = this.newNetwork.name;
          if (this.newNetwork.url !== "") {
            const networks = JSON.parse(localStorage.getItem("networks")) || {};

            const url = this.newNetwork.url;
            networks[networkName.toLocaleLowerCase()] = {
              text: "text-green-400",
              border: "border-green-400",
              name: networkName,
              display: url,
              url: url,
            };
            this.apis = { ...this.apis, ...networks };
            const networksJson = JSON.stringify(networks);
            localStorage.setItem("networks", networksJson);
            this.newNetwork = { name: "", url: "", errors: [] };
          } else {
            this.newNetwork.errors.push(AddNetworkErrorEnums.EmptyUrl);
          }
        } else {
          this.newNetwork.errors.push(AddNetworkErrorEnums.EmptyName);
        }
      }
    },

    setKuberOutput(output: string) {
      this.kuberOutputs.push(output);
      // this.updateOutputScroll("kuber-output");
    },

    setHaskellOutput(output: string) {
      this.haskellOutputs.push(output);
      // this.updateOutputScroll("haskell-output");
    },

    loadHaskellCode(tab: HaskellTabEnums) {
      this.$options.editor.setValue(HaskellCodes[tab]);
      this.$options.editor.updateOptions({
        lineNumbers: true,
        readOnly: true,
      });
    },

    loadKuberCode(tab: KuberTabEnums) {
      if (tab === KuberTabEnums.KuberJson) {
        const storedCode = localStorage.getItem(tab);
        if (storedCode) {
          this.$options.editor.setValue(storedCode);
        } else {
          this.$options.editor.setValue(DefaultKuberCode);
        }
        this.$options.editor.updateOptions({
          lineNumbers: true,
          readOnly: false,
        });
      } else {
        this.$options.editor.setValue(KuberCodes[tab]);
        this.$options.editor.updateOptions({
          lineNumbers: false,
          readOnly: true,
        });
      }
    },

    changeLanguage(language: LanguageEnums) {
      if (this.language !== language) {
        this.showOutputTerminal(false);
      }
      this.language = language;

      if (language == LanguageEnums.Haskell) {
        this.loadHaskellCode(this.haskellSelectedTab);
      } else {
        this.loadKuberCode(this.kuberSelectedTab);
      }

      const model = this.$options.editor.getModel();
      loader.init().then((monaco) => {
        monaco.editor.setModelLanguage(model, language);
      });
    },

    changeUtility(utility: UtilitiesEnums | null) {
      if (!this.utilitiesVisibility && utility != null) {
        this.showUtilities(true);
      }
      this.utility = utility;
    },

    setProvider(provider: CIP30Provider) {
      this.provider = provider;
    },

    handleNetworkDropDown() {
      console.log(this.networkDropdownVisibility);
      this.networkDropdownVisibility = !this.networkDropdownVisibility;
    },

    handleApiSelected(value) {
      this.activeApi = value;
      this.networkDropdownVisibility = false;
      console.log("api selected", value);
      localStorage.setItem("network", JSON.stringify(value));
    },

    onScriptJsonInput(event) {
      this.scriptJson = event.target.value;
    },
    onHexInput(event) {
      this.rawData = event.target.value;
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
      const encoded = Buffer.from(this.rawData).toString("hex");
      this.result = encoded;
    },
    decodeHex() {
      this.errorMsg = "";
      //@ts-ignore
      let val = this.rawData;
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
    getScriptPolicy() {
      // TODO do this with serialization library and not by calling api
      getPolicyIdOfScriptFromKuber(
        this.activeApi.url || this.apis.mainnet.url,
        this.scriptJson
      )
        .catch((err) => alert(err))
        .then((res: string) => {
          this.policyId = res;
        });
    },

    compileCode() {
      console.log("compiling code");
      this.haskellOutputs = [];
      this.isCompiling = true;

      if (this.$options.editor != null) {
        var code = this.$options.editor.getValue();
        code = code.trim();
        localStorage.setItem(LanguageEnums.Haskell, code);
        APIService.compileCode(code).then((response: any) => {
          this.showOutputTerminal(true);
          this.isCompiling = false;
          if (response) {
            if (response.result) {
              response.result.hash;
              const script = response.result.script;
              this.setHaskellOutput("ScriptHash:" + response.result.hash);
              const jsonContent = JSON.stringify(script, null, 5).split("\n");
              jsonContent[0] = "Script :" + jsonContent[0];
              this.haskellOutputs.push(...jsonContent);
            } else {
              console.error(response);
              this.setHaskellOutput(response.output + response.error);
            }
          }
        });
      } else {
        this.isCompiling = false;
      }
      // console.log(this.$options.editor.getValue());
    },

    constructSignAndSubmit(provider: CIP30Provider): any {
      this.kuberOutputs = [];
      this.isCompiling = true;
      this.showOutputTerminal(true);
      if (this.$options.editor != null) {
        var editorContent = this.$options.editor.getValue();
        editorContent = editorContent.replace(DefaultComment, "");
        editorContent = editorContent.trim();
        localStorage.setItem(LanguageEnums.Kuber, editorContent);
        let request;
        try {
          request = JSON.parse(editorContent);
          console.log("success");
        } catch (e: any) {
          notification.setNotification({
            type: "alert",
            message: e.message,
          });
          this.setKuberOutput(e.message);
          this.isCompiling = false;
          return;
        }
        console.log("Using provider", provider);

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
                } else {
                  availableUtxos.push(request.selections);
                  request.selections = availableUtxos;
                }
              } else {
                request.selections = availableUtxos;
              }
            }
            let url = this.activeApi.url;
            if (!url && this.activeApi.name == "Auto") {
              const network = await instance.getNetworkId();
              if (network) {
                url = this.apis.mainnet.url;
              } else {
                url = this.apis["preprod testnet"].url;
              }
            }
            const transactionResponse = await callKuber(url, JSON.stringify(request));
            this.kuberOutputs.push(
              "Fee             : " + transactionResponse.fee + " lovelace"
            );
            this.kuberOutputs.push("Unsigned txHash : " + transactionResponse.txHash);
            this.kuberOutputs.push(
              "Unsigned Tx     : [" +
                transactionResponse.tx.length / 2 +
                " bytes]  " +
                transactionResponse.tx
            );
            const signedTx = await signTx(instance, transactionResponse.tx);
            const signedTxHex = signedTx.to_hex();
            this.kuberOutputs.push(
              "Signed   Tx     : [" + signedTxHex.length / 2 + " bytes]  " + signedTxHex
            );
            this.kuberOutputs.push(
              "Signed  txHash  : "+hash_transaction(signedTx.body()).to_hex()
            );
            return submitTx(instance, signedTx)
              .then(() => {
                this.kuberOutputs.push("✅ Tx Submitted");
              })
              .catch((e) => {
                this.kuberOutputs.push(
                  "❌ Tx submission Failed: " + ((e && (e.message || e.info)) || e)
                );
                notification.setNotification({
                  type: "error",
                  message:
                  (e && (e.message || e.info)) || "Oopsie, Nobody knows what happened",
                });
              });
          })
          .catch((e: any) => {
            console.error("SubmitTx", e);
            let msg=(e && (e.message || e.info)) || "Oopsie, Nobody knows what happened"
            notification.setNotification({
              type: "alert",
              message:msg
            });
            this.kuberOutputs.push("❌ "+msg);
          })
          .finally(() => {
            this.isCompiling = false;
          });
      }
    },

    editorInit() {
      // intializing monaco editor

      loader.init().then((monaco) => {
        var jsonCode = "";
        const storedCode = localStorage.getItem(LanguageEnums.Kuber);
        if (storedCode) {
          jsonCode = [storedCode].join("\n");
        } else {
          jsonCode = [DefaultComment + "\n\n" + DefaultKuberCode].join("\n");
        }
        var modelUri = monaco.Uri.parse("a://b/kuber.json");
        var model = monaco.editor.createModel(jsonCode, "json", modelUri);

        monaco.languages.register({ id: "haskell" });
        monaco.languages.json.jsonDefaults.setDiagnosticsOptions({
          validate: true,
          allowComments: true,
          schemas: [
            {
              uri: "",
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

        this.$options.editor = monaco.editor.create(
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

.disable-text-selection {
  -webkit-touch-callout: none;
  -webkit-user-select: none;
  -khtml-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  user-select: none;
}
.textover {
  position: relative;
  cursor: pointer;
  text-align: start;
  display: inline-block;
}

.textover .text {
  visibility: hidden;
  width: 300px;
  text-align: start;
  background-color: black;
  color: #fff;
  text-align: center;
  padding: 5px 0;
  border-radius: 6px;
  position: absolute;
  z-index: 1;
}

.textover:hover .text {
  visibility: visible;
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
  width: 100%;
  padding: 5px;
  border-radius: 4px;
}

.textarea {
  height: 200px;
  width: 100%;
  padding: 5px;
  border-radius: 4px;
}

textarea:focus {
  outline-width: 0;
}

input:focus {
  outline-width: 0;
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
