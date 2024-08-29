import { defineConfig } from 'vite'

export default defineConfig({
  clearScreen: false,
  optimizeDeps: {
    exclude: ["ezno"]
  },
  base: "/ezno/playground",
  build: {
    outDir: "dist/playground"
  },
  server: {
    fs: { strict: false }
  },
})