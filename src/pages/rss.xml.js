import rss from '@astrojs/rss';
import { getAllContent, getContentUrl, getContentTypeLabel } from '../lib/content.ts';

export async function GET(context) {
  const allContent = await getAllContent();

  return rss({
    title: 'Astro Terminal Theme',
    description: 'A terminal-inspired theme for Astro',
    site: context.site,
    items: allContent.map(({ item, type }) => {
      // Get title based on content type
      let title = item.data.title || '';
      if (type === 'snippets') {
        title = `Snippet: ${item.data.slug}`;
      } else if (type === 'quotations') {
        title = `Quote by ${item.data.author}`;
      }

      return {
        title: title || `${getContentTypeLabel(type)} ${item.data.slug}`,
        pubDate: item.data.createdAt,
        description: item.data.description || '',
        link: getContentUrl(item),
        categories: item.data.tags || [],
      };
    }),
    customData: `<language>en-us</language>`,
  });
}