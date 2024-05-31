import { defineConfig, normalizePath } from 'vite'

export default defineConfig({
  clearScreen: false,
  optimizeDeps: {
    exclude: ["ezno"]
  },
  base: "/playground",
  build: {
    outDir: "dist/playground"
  },
  server: {
    fs: { strict: false }
  },
  // assetsInclude: ["./node_modules/ezno/dist/shared/*.wasm"]
})