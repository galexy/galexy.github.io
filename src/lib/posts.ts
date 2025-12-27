import type { CollectionEntry } from 'astro:content';

/**
 * Extracts date and slug from a post.
 * Tries to parse date from filename (YYYY-MM-DD-slug format),
 * falls back to createdAt frontmatter property.
 */
export function getPostDateAndSlug(post: CollectionEntry<'posts'>) {
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = post.slug.match(datePattern);

  if (match) {
    // Parse date from filename
    const [, year, month, day, slug] = match;
    return {
      year: parseInt(year, 10),
      month: parseInt(month, 10),
      day: parseInt(day, 10),
      slug,
      date: new Date(parseInt(year, 10), parseInt(month, 10) - 1, parseInt(day, 10)),
    };
  }

  // Fall back to createdAt from frontmatter
  const date = new Date(post.data.createdAt);
  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug: post.slug,
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
