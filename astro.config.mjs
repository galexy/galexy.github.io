import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';
import tailwindcss from '@tailwindcss/vite';
import remarkMath from 'remark-math';
import remarkFigureCaption from '@microflash/remark-figure-caption';
import remarkToc from 'remark-toc';
import remarkCollapse from 'remark-collapse';
import rehypeKatex from 'rehype-katex';
import rehypeObsidianImages from './src/plugins/rehype-obsidian-images.mjs';
import rehypeYoutubeEmbed from './src/plugins/rehype-youtube-embed.mjs';

// https://astro.build/config
export default defineConfig({
  site: 'https://flatmap.io',
  integrations: [sitemap()],
  vite: {
    plugins: [tailwindcss()]
  },
  markdown: {
    shikiConfig: {
      theme: 'github-dark',
      wrap: true
    },
    remarkPlugins: [
      remarkMath,
      remarkFigureCaption,
      [remarkToc, { heading: 'table of contents', maxDepth: 3, tight: true }],
      [remarkCollapse, { test: 'Table of Contents', summary: (str) => str }],
    ],
    rehypePlugins: [rehypeKatex, rehypeObsidianImages, rehypeYoutubeEmbed]
  }
});