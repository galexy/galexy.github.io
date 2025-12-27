import type { CollectionEntry } from 'astro:content';

export function getWebmarkDateAndSlug(webmark: CollectionEntry<'webmarks'>) {
  // Always use createdAt from frontmatter for the date (preserves time)
  const date = new Date(webmark.data.createdAt);

  // Try to extract slug from filename pattern YYYY-MM-DD-slug
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = webmark.slug.match(datePattern);
  const slug = match ? match[4] : webmark.slug;

  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug,
    date,
  };
}

export function getWebmarkUrl(webmark: CollectionEntry<'webmarks'>) {
  return `/webmarks/${webmark.slug}`;
}

export function getWebmarkPreview(webmark: CollectionEntry<'webmarks'>, maxLength = 150) {
  // Remove markdown formatting and create preview
  const preview = webmark.body
    .replace(/```[\s\S]*?```/g, '[code]')
    .replace(/!\[([^\]]*)\]\([^)]+\)/g, '') // Remove images
    .replace(/\[([^\]]+)\]\([^)]+\)/g, '$1') // Convert links to text
    .replace(/~~([^~]+)~~/g, '$1') // Remove double-tilde strikethrough
    .replace(/~([^~]+)~/g, '$1') // Remove single-tilde strikethrough
    .replace(/[#*`]/g, '')
    .replace(/\s+/g, ' ')
    .trim();

  return preview.length > maxLength ? preview.substring(0, maxLength) + '...' : preview;
}
