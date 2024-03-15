import { defineConfig, normalizePath } from 'vite'

export default defineConfig({
  clearScreen: false,
  optimizeDeps: {
    exclude: ["ezno"]
  },
  // assetsInclude: ["./node_modules/ezno/dist/shared/*.wasm"]
})