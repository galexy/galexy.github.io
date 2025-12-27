import type { CollectionEntry } from 'astro:content';

/**
 * Extracts date and slug from a post.
 * Tries to parse date from filename (YYYY-MM-DD-slug format),
 * falls back to createdAt frontmatter property.
 */
export function getPostDateAndSlug(post: CollectionEntry<'posts'>) {
  // Always use createdAt from frontmatter for the date (preserves time)
  const date = new Date(post.data.createdAt);

  // Try to extract slug from filename pattern YYYY-MM-DD-slug
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = post.slug.match(datePattern);
  const slug = match ? match[4] : post.slug;

  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug,
    date,
  };
}

/**
 * Generates the URL for a post using date-based routing.
 */
export function getPostUrl(post: CollectionEntry<'posts'>) {
  const { year, month, day, slug } = getPostDateAndSlug(post);
  const monthStr = String(month).padStart(2, '0');
  const dayStr = String(day).padStart(2, '0');
  return `/${year}/${monthStr}/${dayStr}/${slug}`;
}
