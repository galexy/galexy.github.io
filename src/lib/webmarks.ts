import type { CollectionEntry } from 'astro:content';

export function getWebmarkDateAndSlug(webmark: CollectionEntry<'webmarks'>) {
  // Try to extract date from slug pattern: YYYY-MM-DD-slug
  const datePattern = /^(\d{4})-(\d{2})-(\d{2})-(.+)$/;
  const match = webmark.slug.match(datePattern);

  if (match) {
    const [, year, month, day, slug] = match;
    return {
      year: parseInt(year, 10),
      month: parseInt(month, 10),
      day: parseInt(day, 10),
      slug,
      date: new Date(parseInt(year, 10), parseInt(month, 10) - 1, parseInt(day, 10)),
    };
  }

  // Fallback to createdAt
  const date = new Date(webmark.data.createdAt);
  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDate(),
    slug: webmark.slug,
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
    .replace(/[#*`]/g, '')
    .replace(/\s+/g, ' ')
    .trim();

  return preview.length > maxLength ? preview.substring(0, maxLength) + '...' : preview;
}
