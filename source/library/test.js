// fetch the wasm file and instantiate it
import { readFileSync } from 'node:fs';
const wasm = readFileSync('./malloc.wasm');

const { instance } = await WebAssembly.instantiate(wasm);

// call the exported function
console.log(instance.exports.test_unreachable(0));
