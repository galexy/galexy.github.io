import { defineConfig } from 'astro/config';
import tailwindcss from '@tailwindcss/vite';
import remarkMath from 'remark-math';
import remarkFigureCaption from '@microflash/remark-figure-caption';
import rehypeKatex from 'rehype-katex';
import rehypeObsidianImages from './src/plugins/rehype-obsidian-images.mjs';

// https://astro.build/config
export default defineConfig({
  site: 'https://flatmap.io',
  vite: {
    plugins: [tailwindcss()]
  },
  markdown: {
    shikiConfig: {
      theme: 'github-dark',
      wrap: true
    },
    remarkPlugins: [remarkMath, remarkFigureCaption],
    rehypePlugins: [rehypeKatex, rehypeObsidianImages]
  }
});