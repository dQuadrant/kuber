// @ts-nocheck

export const SchemaKuber = {
  $schema: "http://json-schema.org/draft-04/schema#",
  type: "object",
  properties: {
    selections: {
         type: "array",
         items: [
          {
            anyOf: [
              {"$ref":"#/$defs/UtxoString"},
              {"$ref":"#/$defs/AddressString"},
              {"$ref":"#/$defs/CborHexString"},
            ]
          }
         ]
    },
    outputs: {
      type: "array",
      items: [
        {
          type: "object",
          properties: {
            address:{"$ref":"#/$defs/AddressString"},
            value: {
              type: ["string","integer"],
            },
            addChange: {
              type: "boolean"
            },
            deductFee: {
              type: "boolean",
            },
            datum: {
              "$ref": "#/$defs/ScriptData",
            },
            insuffientUtxoAda: {
              type: "string",
              enum: ["error","increase","drop"]
            },
            datumHash: {
              type: "string"
            },
            inlineDatum: {
              type: "boolean",
            },
            script: {"$ref":"#/$defs/AnyScript"}
          },
        },
      ],
    },
    metadata: {
      type: "object",
      properties: {
        "721": {
          type: "object",
          patternProperties: {
            "[0-9a-f]{32}": {
              type: "object",
              patternProperties: {
                ".{0,64}": {
                  type: "object",
                  properties: {
                    name: {
                      type: "string",
                    },
                    image: {
                      type: "string",
                    },
                    artist: {
                      type: "string",
                    },
                    mediaType: {
                      type: "string",
                    },
                    files: {},
                    tags:{},
                    traits:{}
                  },
                },
              },
              additionalProperties: false
            },
            additionalProperties: false
          },
          additionalProperties: false
        }
      },
      patternProperties: {
        "[0-9]+":{}
     },
     additionalProperties: false
    },
    mint: {
      type: "array",
      items: [
        {
          anyOf: [
            { "$ref": "#/$defs/MintSimpleScript"},
            { "$ref": "#/$defs/MintPlutusScript"},
          ]
        }
      ],
    },
    inputs: {
      type: "array",
      items: [{ 
        anyOf :[
          {"$ref": "#/$defs/InputObject"},
          {"$ref": "#/$defs/UtxoString"},  
          {"$ref": "#/$defs/AddressString"},  
          {"$ref": "#/$defs/CborHexString"},  
        ]
      }]
    },
    validityStart: {
      type: "integer"
    },
    validityEnd: {
      type: "integer"
    },
    validityEndSlot:{
      type: "integer"
    },
    validityStartSlot:{
      type: "integer"
    },
    changeAddress:{
      "$ref": "#/$defs/AddressString"
    },
    collaterals:{
      anyOf:[
      {"$ref":"#/$defs/Utxo"},
      {"$ref":"#/$defs/CborHexString"},
      ]
    },
    referenceInputs:{
      "$ref":"#/$defs/UtxoString"
    },
    signatures:{
      "$ref":"#/$defs/AddressString"
    }

  },
  "$defs": {
    MintPlutusScript:{
      type: "object",
      properties: {
        redeemer: {"$ref":"#/$defs/ScriptData",required: true},
        script: {
          "$ref": "#/$defs/PlutusScriptOrRef",
          required: true
        },
        amount: {
          type: "object",
          required: true,
          "patternProperties": {
            ".{0,128}": {
                type: "integer",   
              },
          }, 
        "additionalProperties": false
        },
      },
      "additionalProperties": false
    },
    MintSimpleScript:{
      type: "object",
      properties: {
        script: {
          "$ref": "#/$defs/SimpleScriptOrRef",
          required: true
        },
        amount: {
          type: "object",
          "patternProperties": {
            ".{0,128}": {
                type: "integer",   
              },
          }, 
          required: true,
          additionalProperties: false
        },
      },
      additionalProperties: false

    },
    InputObject: {
      type: "object",
      properties:{
        utxo: {
          "$ref": "#/$defs/Utxo",
          required: true
        },
        script: {
          "$ref":"#/$defs/PlutusScriptOrRef", 
        },
        redeemer: {
          "$ref": "#/$defs/ScriptData",
        },
        datum: {
          "$ref": "#/$defs/ScriptData",
        },
      },
      "additionalProperties": false,    
    },
    Utxo :{
      oneOf:[
        {"$ref": "#/$defs/UtxoString"},
        {"$ref": "#/$defs/UtxoObject"},

      ]
    },
    UtxoString:{
      type: "string",
      pattern: "[a-f0-9A-F]{64}#\\d",
      required: true
    },
    UtxoObject:{
      type: "object",
      properties:{
        hash: {
          type: "string",
          pattern: "[a-f0-9A-F]{64}",
          required: true
        },
        index:{
          type: "integer",
          required: true
        },
      },
      additionalProperties: false
    }
    ,
    AddressString:{
      type: "string",
      pattern: "^addr[_a-z1-9]+[0-9a-z]+",
      required: true
    },
    CborHexString:{
      type: "string",
      pattern: "[0-9a-fA-F]{5,}",
      required: true
    },
    PlutusScript:{
      type: "object",
      properties: {
        type: {
          type: "string",
          enum: ["PlutusScriptV2", "PlutusScriptV1"],
          required: true
        },
        description: {
          type: "string",required: true
        },
        cborHex: {
          type: "string",
          required: true
        },
      },
    },
    PlutusScriptOrRef:{
      oneOf:[
        {"$ref":"#/$defs/PlutusScript"},
        {"$ref":"#/$defs/UtxoString"},
      ]
    },
    SimpleScript:{
      oneOf: [
        {"$ref":"#/$defs/SimpleScriptSig"},
        {"$ref": "#/$defs/SimpleScriptTimeSlot"},
        {"$ref":"#/$defs/SimpleScriptAnyAll"},
        {"$ref":"#/$defs/SimpleScriptAnyM"},
      ]
    },
    SimpleScriptOrRef: {
      oneOf:[
        {"$ref":"#/$defs/SimpleScriptSig"},
        {"$ref":"#/$defs/SimpleScriptTimeSlot"},
        {"$ref":"#/$defs/SimpleScriptAnyAll"},
        {"$ref":"#/$defs/SimpleScriptAnyM"},
        {"$ref":"#/$defs/UtxoString"},
      ]
    },
    AnyScriptOrRef: {
      oneOf: [
        {"$ref":"#/$defs/UtxoString"},
        {"$ref":"#/$defs/PlutusScript"},
        {"$ref":"#/$defs/SimpleScriptSig"},
        {"$ref": "#/$defs/SimpleScriptTimeSlot"},
        {"$ref":"#/$defs/SimpleScriptAnyAll"},
        {"$ref":"#/$defs/SimpleScriptAnyM"},
      ]
    },
    AnyScript: {
      oneOf: [
        {"$ref":"#/$defs/PlutusScript"},
        {"$ref":"#/$defs/SimpleScriptSig"},
        {"$ref": "#/$defs/SimpleScriptTimeSlot"},
        {"$ref":"#/$defs/SimpleScriptAnyAll"},
        {"$ref":"#/$defs/SimpleScriptAnyM"},
      ]
    },
    SimpleScriptSig:{
      type: "object",
      properties:{
        type: {type :"string", enum:["sig"],required: true},
        keyHash: {type: "string",required: true}
      },
      additionalProperties: false,
    },
    SimpleScriptTimeSlot:{
      type: "object",
      properties:{
        type: {type :"string", enum:["before","after"],required: true},
        slot: {type: "string",required: true}
      },
      additionalProperties: false,
    },
    SimpleScriptAnyAll:{
      type: "object",
      properties:{
        type: {type :"string", enum:["all","any"],required: true},
        scripts: {"$ref": "#/$defs/SimpleScript",required: true}
      },
      additionalProperties: false,
    },
    SimpleScriptAnyM:{
      type: "object",
      properties:{
        type: {type :"string", enum:["atLeast"],required: true},
        required: {type: "integer",required: true},
        scripts: {
          type: "array",
          items: [{
            "$ref": "#/$defs/SimpleScript",
            required: true
          }],
          required: true
        }
      },
      additionalProperties: false,
    },
    ScriptData:{
      oneOf: [
        {"$ref":"#/$defs/datumConstructor"},
        {"$ref": "#/$defs/datumInt"},
        {"$ref":"#/$defs/datumBytes"},
        {"$ref":"#/$defs/datumList"},
        {"$ref":"#/$defs/datumMap"}]
    },
    datumInt: {
      type: "object",
      properties:{
        int: {type: "integer",required: true}
      },
      "additionalProperties": false,
    },
    datumBytes:{
      type: "object",
      properties:{
        bytes: {type: "string",required:true}
      },
      "additionalProperties": false,
    },
    datumList: {
      type: "object",
      properties:{
        "list": {
          type: "array",
          required: true,
          items:{
            type: {
              "$ref":"#/$defs/ScriptData"
            }
          }
        }
      },
      "additionalProperties": false,
    },
    datumMap:{
      type: "object",
      properties:{
        map:{
          type:"array",
          required: true,
          items:{
            type: "object",
            properties:{
              "k":{type:{"$ref":"#/$defs/ScriptData"},required: true},
              "v":{type: {"$ref":"#/$defs/ScriptData"},required: true}
            },
            "additionalProperties": false,
          }
        }
      },
      "additionalProperties": false,
    },
    datumConstructor: {
      type:"object",
      properties:{
        "constructor": {type: "integer",required:true},
        "fields": {
          type: "array",
          items: {
            type: {"$ref":"#/$defs/ScriptData"},
          },  
          required: true
        }
      },
      "additionalProperties": false,
    }
  }
}
