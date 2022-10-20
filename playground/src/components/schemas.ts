// @ts-nocheck

export const SchemaKuber = {
  $schema: "http://json-schema.org/draft-04/schema#",
  type: "object",
  properties: {
    selections: {
      type: "array",
      items: [
        {
          type: "string",
        },
      ],
    },
    outputs: {
      type: "array",
      items: [
        {
          type: "object",
          properties: {
            address: {
              type: "string",
            },
            value: {
              type: "string",
            },
            deductFee: {
              type: "boolean",
            },
            datum: {
              type: "object",
              properties: {
                fields: {
                  type: "array",
                  items: [
                    {
                      type: "object",
                      properties: {
                        fields: {
                          type: "array",
                          items: [
                            {
                              type: "object",
                              properties: {
                                fields: {
                                  type: "array",
                                  items: [
                                    {
                                      type: "object",
                                      properties: {
                                        bytes: {
                                          type: "string",
                                        },
                                      },
                                      required: ["bytes"],
                                    },
                                  ],
                                },
                                constructor: {
                                  type: "integer",
                                },
                              },
                            },
                            {
                              type: "object",
                              properties: {
                                fields: {
                                  type: "array",
                                  items: {},
                                },
                                constructor: {
                                  type: "integer",
                                },
                              },
                            },
                          ],
                        },
                        constructor: {
                          type: "integer",
                        },
                      },
                    },
                    {
                      type: "object",
                      properties: {
                        int: {
                          type: "integer",
                        },
                      },
                    },
                  ],
                },
                constructor: {
                  type: "integer",
                },
              },
            },
            inline: {
              type: "boolean",
            },
          },
        },
      ],
    },
    metadata: {
      type: "object",
      properties: {
        "0": {
          type: "object",
          properties: {
            library: {
              type: "string",
            },
            version: {
              type: "string",
            },
          },
        },
        "1": {
          type: "string",
        },
        "721": {
          type: "object",
          properties: {
            a6d263a3c017823752bcd42685c78fcb11f7a5759d9c956f51b64318: {
              type: "object",
              properties: {
                test1: {
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
                  },
                },
              },
            },
          },
        },
        "20": {
          type: "object",
          properties: {
            a6d263a3c017823752bcd42685c78fcb11f7a5759d9c956f51b64318: {
              type: "object",
              properties: {
                test2: {
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
                  },
                },
              },
            },
          },
        },
      },
    },
    mint: {
      type: "array",
      items: [
        {
          type: "object",
          properties: {
            script: {
              type: "object",
              properties: {
                type: {
                  type: "string",
                },
                keyHash: {
                  type: "string",
                },
                slot: {
                  type: "integer",
                },
                scripts: {
                  type: "array",
                  items: [
                    {
                      type: "object",
                      properties: {
                        keyHash: {
                          type: "string",
                        },
                        type: {
                          type: "string",
                        },
                      },
                    },
                    {
                      type: "object",
                      properties: {
                        slot: {
                          type: "integer",
                        },
                        type: {
                          type: "string",
                        },
                      },
                    },
                  ],
                },
                cborHex: {
                  type: "string",
                },
                redeemer: {
                  type: "integer",
                },
              },
            },
            amount: {
              type: "object",
              properties: {
                test1: {
                  type: "integer",
                },
                test2: {
                  type: "integer",
                },
                Token1: {
                  type: "integer",
                },
                Token2: {
                  type: "integer",
                },
              },
            },
          },
        },
      ],
    },
    inputs: {
      type: "array",
      items: [
        {
          type: "object",
          properties: {
            utxo: {
              type: "string",
            },
            script: {
              type: "object",
              properties: {
                type: {
                  type: "string",
                },
                description: {
                  type: "string",
                },
                cborHex: {
                  type: "string",
                },
              },
            },
            redeemer: {
              type: "object",
              properties: {
                fields: {
                  type: "array",
                  items: {},
                },
                constructor: {
                  type: "integer",
                },
              },
            },
            datum: {
              type: "object",
              properties: {
                constructor: {
                  type: "integer",
                },
                fields: {
                  type: "array",
                  items: [
                    {
                      type: "object",
                      properties: {
                        constructor: {
                          type: "integer",
                        },
                        fields: {
                          type: "array",
                          items: [
                            {
                              type: "object",
                              properties: {
                                constructor: {
                                  type: "integer",
                                },
                                fields: {
                                  type: "array",
                                  items: [
                                    {
                                      type: "object",
                                      properties: {
                                        bytes: {
                                          type: "string",
                                        },
                                      },
                                    },
                                  ],
                                },
                              },
                            },
                            {
                              type: "object",
                              properties: {
                                constructor: {
                                  type: "integer",
                                },
                                fields: {
                                  type: "array",
                                  items: {},
                                },
                              },
                            },
                          ],
                        },
                      },
                      required: ["constructor", "fields"],
                    },
                    {
                      type: "object",
                      properties: {
                        int: {
                          type: "integer",
                        },
                      },
                    },
                  ],
                },
              },
            },
          },
        },
      ],
    },
  },
};
