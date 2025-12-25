import { getCollection } from 'astro:content';
import type { CollectionEntry } from 'astro:content';

// Type for unified content items
export type ContentType = 'posts' | 'snippets' | 'quotations' | 'til' | 'webmarks';

export interface UnifiedContentItem {
  item: CollectionEntry<any>;
  type: ContentType;
  date: Date;
}

/**
 * Fetch all content across all collections
 */
export async function getAllContent(includeDrafts = false): Promise<UnifiedContentItem[]> {
  const [posts, snippets, quotations, til, webmarks] = await Promise.all([
    getCollection('posts', ({ data }) => includeDrafts || !data.draft),
    getCollection('snippets', ({ data }) => includeDrafts || !data.draft),
    getCollection('quotations', ({ data }) => includeDrafts || !data.draft),
    getCollection('til', ({ data }) => includeDrafts || !data.draft),
    getCollection('webmarks', ({ data }) => includeDrafts || !data.draft),
  ]);

  const allContent: UnifiedContentItem[] = [
    ...posts.map(item => ({ item, type: 'posts' as ContentType, date: item.data.createdAt })),
    ...snippets.map(item => ({ item, type: 'snippets' as ContentType, date: item.data.createdAt })),
    ...quotations.map(item => ({ item, type: 'quotations' as ContentType, date: item.data.createdAt })),
    ...til.map(item => ({ item, type: 'til' as ContentType, date: item.data.createdAt })),
    ...webmarks.map(item => ({ item, type: 'webmarks' as ContentType, date: item.data.createdAt })),
  ];

  return allContent.sort((a, b) => b.date.valueOf() - a.date.valueOf());
}

/**
 * Group content by date
 */
export function groupByDate(content: UnifiedContentItem[]): Map<string, UnifiedContentItem[]> {
  const groups = new Map<string, UnifiedContentItem[]>();

  for (const item of content) {
    const dateKey = item.date.toISOString().split('T')[0]; // YYYY-MM-DD
    if (!groups.has(dateKey)) {
      groups.set(dateKey, []);
    }
    groups.get(dateKey)!.push(item);
  }

  return groups;
}

/**
 * Get all unique tags across all content types
 */
export async function getAllTags(): Promise<Map<string, number>> {
  const allContent = await getAllContent();
  const tagCounts = new Map<string, number>();

  for (const { item } of allContent) {
    for (const tag of item.data.tags || []) {
      tagCounts.set(tag, (tagCounts.get(tag) || 0) + 1);
    }
  }

  return tagCounts;
}

/**
 * Filter content by tag
 */
export function filterByTag(content: UnifiedContentItem[], tag: string): UnifiedContentItem[] {
  return content.filter(({ item }) =>
    item.data.tags?.includes(tag)
  );
}

/**
 * Generate date-based URL for any content item
 */
export function getContentUrl(item: CollectionEntry<any>): string {
  const date = item.data.createdAt;
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  const slug = item.data.slug;

  return `/${year}/${month}/${day}/${slug}`;
}

/**
 * Get content type label for display
 */
export function getContentTypeLabel(type: ContentType): string {
  const labels: Record<ContentType, string> = {
    posts: 'Post:',
    snippets: 'Snippet:',
    quotations: 'Quote:',
    til: 'TIL:',
    webmarks: 'Bookmark:',
  };

  return labels[type];
}
