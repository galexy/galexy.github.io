import type { CollectionEntry } from 'astro:content';

/**
 * Extracts date and slug from a quotation.
 * Tries to parse date from filename (YYYY-MM-DD-slug format),
 * falls back to createdAt frontmatter property.
 */
export function getQuotationDateAndSlug(quotation: CollectionEntry<'quotations'>) {
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = quotation.slug.match(datePattern);

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
  const date = new Date(quotation.data.createdAt);
  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug: quotation.slug,
    date,
  };
}

/**
 * Generates the URL for a quotation.
 */
export function getQuotationUrl(quotation: CollectionEntry<'quotations'>) {
  return `/quotations/${quotation.slug}`;
}

/**
 * Creates a preview from quotation body.
 */
export function getQuotationPreview(quotation: CollectionEntry<'quotations'>, maxLength = 150) {
  const preview = quotation.body
    .replace(/!\[([^\]]*)\]\([^)]+\)/g, '') // Remove images
    .replace(/\[([^\]]+)\]\([^)]+\)/g, '$1') // Convert links to text
    .replace(/~~([^~]+)~~/g, '$1') // Remove double-tilde strikethrough
    .replace(/~([^~]+)~/g, '$1') // Remove single-tilde strikethrough
    .replace(/[#*`]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
  return preview.length > maxLength ? preview.substring(0, maxLength) + '...' : preview;
}
