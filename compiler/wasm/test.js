const fs = require("fs");

async function run() {
   const { instance } = await WebAssembly.instantiate(
      fs.readFileSync("./test.wasm")
   );
   // @ts-ignore
   // console.log(instance.exports.fibonacci(1));
}

run().then(() => {
   console.log("Ran module.");
});
