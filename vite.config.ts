import { defineConfig } from 'vite';

export default defineConfig({
  root: './source/website/pages',
  publicDir: '../public',
  build: {
    outDir: '../../../dist',
    emptyOutDir: true,
  },
});
