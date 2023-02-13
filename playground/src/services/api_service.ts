export default class APIService {
  static async compileCode(code: string) {
    const result= {"code":0,"error":"",
    "output":"Up to date\n====BEGINSCRIPT====\n{\"hash\":\" 3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712\",\"script\":{\"cborHex\":\"49480100002221200101\",\"description\":\"\",\"type\":\"PlutusScriptV2\"}}\n",
    "result":{
      "hash":" 3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712",
      "script":{"cborHex":"49480100002221200101","description":"","type":"PlutusScriptV2"}},
      "success":true}
    return new Promise(resolve => setTimeout(resolve, 1000, result));
  }
}
