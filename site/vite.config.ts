import { defineConfig } from 'vite';

export default defineConfig({
  root: './pages',
  build: {
    outDir: '../dist',
    emptyOutDir: true,
  },
});
