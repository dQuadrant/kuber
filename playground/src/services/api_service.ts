export default class APIService {
  static async compileCode(code: string) {
    try {
      const response = await fetch(
        import.meta.env.VITE_COMPILER_API + "compile",
        {
          method: "POST",
          headers: {
            "content-type": "application/json",
            // accept: "application/json",
          },
          body: JSON.stringify({ code: code }),
        }
      );
      if (response && response.status == 200) {
        const res = await response.json();
        return res;
      }
    } catch (err) {
      return err;
    }
  }
}
