import wasm_init, { initSync } from "../build/ezno_lib.js";

export const init = wasm_init;
export const init_sync = initSync;
export * from "../build/ezno_lib.js";