const fs = require("fs");

const wasm = {
   /**
    * Extracts a Javascript string from a section of Webassembly memory.
    * @param {WebAssembly.Memory} memory
    * @param {number} offset
    * @param {number} length
    */
   extractString(memory, offset, length) {
      const bytes = new Uint8Array(memory.buffer, offset, length);
      const decoded = new TextDecoder("utf-8").decode(bytes);
      return decoded;
   },
};

/**
 * Returns a webassembly import object.
 * @returns {WebAssembly.Imports}
 */
function makeImportObject() {
   const memory = new WebAssembly.Memory({ initial: 1 });
   return {
      io: {
         /**
          * Prints a UTF8 string to the console.
          * @param {number} offset The start of the string.
          * @param {number} length The length of the string.
          */
         print(offset, length) {
            const string = wasm.extractString(memory, offset, length);
            console.log(string);
         },
      },
      mem: {
         memory,
      },
   };
}

async function run() {
   const wasmBuffer = fs.readFileSync("./test.wasm");
   const { instance } = await WebAssembly.instantiate(
      wasmBuffer,
      makeImportObject()
   );
   // @ts-ignore
   instance.exports.writeHi();
}

run().then(() => {
   console.log("Ran module.");
});
