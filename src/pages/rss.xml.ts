import rss from '@astrojs/rss';
import { getCollection } from 'astro:content';
import type { APIContext } from 'astro';
import { getPostUrl } from '../lib/posts';
import { getSnippetUrl, getSnippetPreview, getSnippetDateAndSlug } from '../lib/snippets';
import { getQuotationUrl, getQuotationPreview, getQuotationDateAndSlug } from '../lib/quotations';
import { getTilUrl, getTilPreview, getTilDateAndSlug } from '../lib/til';
import { getWebmarkUrl, getWebmarkPreview, getWebmarkDateAndSlug } from '../lib/webmarks';

export async function GET(context: APIContext) {
  const posts = await getCollection('posts', ({ data }) => data.draft !== true);
  const snippets = await getCollection('snippets', ({ data }) => data.draft !== true);
  const quotations = await getCollection('quotations', ({ data }) => data.draft !== true);
  const tils = await getCollection('til', ({ data }) => data.draft !== true);
  const webmarks = await getCollection('webmarks', ({ data }) => data.draft !== true);

  const items = [
    ...posts.map((post) => ({
      title: post.data.title,
      description: post.data.description,
      pubDate: post.data.createdAt,
      link: getPostUrl(post),
    })),
    ...snippets.map((snippet) => ({
      title: `Snippet: ${getSnippetDateAndSlug(snippet).slug.replace(/-/g, ' ')}`,
      description: getSnippetPreview(snippet, 300),
      pubDate: snippet.data.createdAt,
      link: getSnippetUrl(snippet),
    })),
    ...quotations.map((quotation) => ({
      title: `Quote from ${quotation.data.author}`,
      description: getQuotationPreview(quotation, 300),
      pubDate: quotation.data.createdAt,
      link: getQuotationUrl(quotation),
    })),
    ...tils.map((til) => ({
      title: `TIL: ${getTilDateAndSlug(til).slug.replace(/-/g, ' ')}`,
      description: getTilPreview(til, 300),
      pubDate: til.data.createdAt,
      link: getTilUrl(til),
    })),
    ...webmarks.map((webmark) => ({
      title: `Webmark: ${webmark.data.title}`,
      description: getWebmarkPreview(webmark, 300),
      pubDate: webmark.data.createdAt,
      link: getWebmarkUrl(webmark),
    })),
  ].sort((a, b) => b.pubDate.getTime() - a.pubDate.getTime());

  return rss({
    title: 'flatMap.io blog',
    description: 'Learning in public',
    site: context.site!,
    items,
  });
}
