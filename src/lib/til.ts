import type { CollectionEntry } from 'astro:content';

/**
 * Extracts date and slug from a TIL.
 * Tries to parse date from filename (YYYY-MM-DD-slug format),
 * falls back to createdAt frontmatter property.
 */
export function getTilDateAndSlug(til: CollectionEntry<'til'>) {
  // Always use createdAt from frontmatter for the date (preserves time)
  const date = new Date(til.data.createdAt);

  // Try to extract slug from filename pattern YYYY-MM-DD-slug
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = til.slug.match(datePattern);
  const slug = match ? match[4] : til.slug;

  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug,
    date,
  };
}

/**
 * Generates the URL for a TIL.
 */
export function getTilUrl(til: CollectionEntry<'til'>) {
  return `/til/${til.slug}`;
}

/**
 * Creates a preview from TIL body.
 */
export function getTilPreview(til: CollectionEntry<'til'>, maxLength = 150) {
  const preview = til.body
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
