import type { CollectionEntry } from 'astro:content';

/**
 * Extracts date and slug from a snippet.
 * Tries to parse date from filename (YYYY-MM-DD-slug format),
 * falls back to createdAt frontmatter property.
 */
export function getSnippetDateAndSlug(snippet: CollectionEntry<'snippets'>) {
  // Always use createdAt from frontmatter for the date (preserves time)
  const date = new Date(snippet.data.createdAt);

  // Try to extract slug from filename pattern YYYY-MM-DD-slug
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = snippet.slug.match(datePattern);
  const slug = match ? match[4] : snippet.slug;

  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug,
    date,
  };
}

/**
 * Generates the URL for a snippet.
 */
export function getSnippetUrl(snippet: CollectionEntry<'snippets'>) {
  return `/snippets/${snippet.slug}`;
}

/**
 * Creates a preview from snippet body by removing code blocks and markdown syntax.
 */
export function getSnippetPreview(snippet: CollectionEntry<'snippets'>, maxLength = 150) {
  const preview = snippet.body
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
