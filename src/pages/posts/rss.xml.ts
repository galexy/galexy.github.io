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
    title: 'reasoning trace - Posts',
    description: 'Blog posts from reasoning trace',
    site: context.site!,
    items,
  });
}
