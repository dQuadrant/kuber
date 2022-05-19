<script setup lang="ts">
import { VAceEditor } from "vue3-ace-editor";
import ace from "ace-builds";
import workerJsonUrl from "ace-builds/src-noconflict/mode-json";
// @ts-ignore (for some reason ide is giving error on @/ imports)
import { callKuberAndSubmit, listProviders } from "@/scripts/wallet";
import type { CIP30Instace, CIP30Provider } from "@/types";
// @ts-ignore
ace.config.setModuleUrl("ace/mode/json_worker", workerJsonUrl);
</script>

<template>
  <div class="container items-center">
    <vue-notification-list position="top-right"></vue-notification-list>

    <div>
      <div>
        <span class="text-blue-800"> Create Tx with </span>
        <span>
          <button
            v-for="provider in providers"
            :key="provider.name"
            class="ml-3 bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-0.5 px-1.5 border border-blue-500 hover:border-transparent rounded"
            @click="submitTx(provider)"
          >
            <!-- @ts-ignores -->
            <img style="display: inline; height: 1em; width: 1em" :src="provider.icon" />
            <span>{{ provider.name }}</span>
          </button>

          <div class="ml-2 mt-1 form-check form-check text-sm">
            <input
              ref="addUtxo"
              class="form-check-input h-4 w-4 border border-gray-300 rounded-sm bg-white checked:bg-blue-600 checked:border-blue-600 focus:outline-none transition duration-200 mt-1 align-top bg-no-repeat bg-center bg-contain float-left mr-2 cursor-pointer"
              type="checkbox"
              id="inlineCheckbox1"
              :checked="addSelections"
              value="addSelections"
            />
            <label
              class="form-check-label inline-block text-gray-500"
              for="inlineCheckbox1"
            >
              Add Wallet UTxOs in selection</label
            >
          </div>
        </span>
      </div>
    </div>

    <v-ace-editor
      v-model:value="content"
      @init="editorInit"
      lang="json"
      theme="chrome"
      style="display: flex; min-height: 100%; height: 80vh; width: 100%"
    />
  </div>
</template>
<script lang="ts">
import * as _notification from "@dafcoe/vue-notification";

const notification = _notification.useNotificationStore();
export default {
  mounted() {
        this.providers = listProviders();
  },
  data() {
    const providers: Array<CIP30Provider> = [];

    return {
      content: "",
      providers: providers,
      addSelections: true,
      editor: null,
      interval: 0,
    };
  },
  beforeUnmount() {
    this.interval && clearInterval(this.interval);
  },
  methods: {
    submitTx(provider: CIP30Provider) {
      this.save();
      console.log("submitTx", this.content);
      let request
      try{
       request = JSON.parse(this.content);
      }catch(e:any){
        notification.setNotification({
            type: "alert",
            message: e.message ,
          });
          return;
      }
      return provider
        .enable()
        .then((instance: CIP30Instace) => {
          if (this.addSelections) {
            return instance.getUsedAddresses().then((usedAddresses) => {
              if (request.selections) {
                if (typeof request.selections.push === "function") {
                  console.log("usedAddresses", usedAddresses);
                  usedAddresses.forEach((v) => {
                    request.selections.push(v);
                  });
                }
              } else {
                request.selections = usedAddresses;
              }
              return callKuberAndSubmit(instance, JSON.stringify(request));
            });
          } else {
            return callKuberAndSubmit(instance, JSON.stringify(this.content));
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
      const session = v.getSession();
      session.setTabSize(2);
      session.setOptions({
        basicAutocompletion: true,
        useWorker: true,
      });
      session.setUseWrapMode(true);
      this.content = localStorage.getItem("editor.content") || "{\n\n}";
      this.interval = setInterval(this.save, 2000);
    },
    save() {
      localStorage.setItem("editor.content", this.content);
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
