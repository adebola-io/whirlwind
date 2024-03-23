import { defineConfig } from 'vite';

export default defineConfig({
  root: './source/website/pages',
  build: {
    outDir: '../../../dist',
    emptyOutDir: true,
  },
});
