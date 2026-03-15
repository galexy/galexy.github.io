import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';
import tailwindcss from '@tailwindcss/vite';
import remarkMath from 'remark-math';
import remarkFigureCaption from '@microflash/remark-figure-caption';
import rehypeKatex from 'rehype-katex';
import rehypeObsidianImages from './src/plugins/rehype-obsidian-images.mjs';
import rehypeYoutubeEmbed from './src/plugins/rehype-youtube-embed.mjs';
import {
  transformerNotationDiff,
  transformerNotationHighlight,
  transformerNotationWordHighlight,
  transformerMetaHighlight,
} from '@shikijs/transformers';

/** Custom transformer to extract title="filename" from code block meta */
function transformerMetaTitle() {
  return {
    name: 'meta-title',
    pre(node) {
      const meta = this.options.meta?.__raw;
      if (!meta) return;
      const match = meta.match(/title="([^"]+)"/);
      if (match) {
        node.properties['data-title'] = match[1];
      }
    },
  };
}

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
      wrap: true,
      transformers: [
        transformerNotationDiff(),
        transformerNotationHighlight(),
        transformerNotationWordHighlight(),
        transformerMetaHighlight(),
        transformerMetaTitle(),
      ],
    },
    remarkPlugins: [remarkMath, remarkFigureCaption],
    rehypePlugins: [rehypeKatex, rehypeObsidianImages, rehypeYoutubeEmbed]
  }
});