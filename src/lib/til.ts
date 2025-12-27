import type { CollectionEntry } from 'astro:content';

/**
 * Extracts date and slug from a TIL.
 * Tries to parse date from filename (YYYY-MM-DD-slug format),
 * falls back to createdAt frontmatter property.
 */
export function getTilDateAndSlug(til: CollectionEntry<'til'>) {
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = til.slug.match(datePattern);

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
  const date = new Date(til.data.createdAt);
  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug: til.slug,
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
    .replace(/[#*`]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
  return preview.length > maxLength ? preview.substring(0, maxLength) + '...' : preview;
}
