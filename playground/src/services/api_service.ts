export default class APIService {
  static async compileCode(code: string) {
    const response = await fetch(import.meta.env.VITE_COMPILER_API, {
      method: "POST",
      mode: "cors",
      headers: {
        "content-type": "application/json",
        accept: "application/json",
      },
      body: JSON.stringify({ code: code }),
    });
    console.log(response);
  }
}
