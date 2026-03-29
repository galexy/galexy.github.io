import rss from '@astrojs/rss';
import { getCollection } from 'astro:content';
import type { APIContext } from 'astro';
import { getPostUrl } from '../../lib/posts';

export async function GET(context: APIContext) {
  const posts = await getCollection('posts', ({ data }) => data.draft !== true);

  const items = posts
    .map((post) => ({
      title: post.data.title,
      description: post.data.description,
      pubDate: post.data.createdAt,
      link: getPostUrl(post),
    }))
    .sort((a, b) => b.pubDate.getTime() - a.pubDate.getTime());

  return rss({
    title: 'trace.reasoning.net blog - Posts',
    description: 'Blog posts from trace.reasoning.net',
    site: context.site!,
    items,
  });
}
