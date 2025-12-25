import { defineCollection, z } from 'astro:content';

// Common schema fields shared across all content types
const commonFields = {
  createdAt: z.coerce.date(),
  tags: z.array(z.string()).default([]),
  slug: z.string(),
  draft: z.boolean().default(false),
};

// Posts - Long-form blog posts
const posts = defineCollection({
  type: 'content',
  schema: z.object({
    ...commonFields,
    title: z.string(),
    description: z.string().optional(),
    updatedDate: z.coerce.date().optional(),
    author: z.string().optional(),
    image: z.string().optional(),
    externalLink: z.string().optional(),
    // Backward compatibility: support both pubDate and createdAt
    pubDate: z.coerce.date().optional(),
  }).transform((data) => {
    // If pubDate exists but createdAt doesn't, use pubDate as createdAt
    if (data.pubDate && !data.createdAt) {
      data.createdAt = data.pubDate;
    }
    return data;
  }),
});

// Snippets - Short tweet-like content
const snippets = defineCollection({
  type: 'content',
  schema: z.object({
    ...commonFields,
    // No title required - content speaks for itself
    // Markdown content will be the snippet body
  }),
});

// Quotations - Captured quotes with attribution
const quotations = defineCollection({
  type: 'content',
  schema: z.object({
    ...commonFields,
    quote: z.string(), // The actual quote text
    author: z.string(), // Who said it
    source: z.string().optional(), // Book, article, speech, etc.
    url: z.string().url().optional(), // Link to source
    highlights: z.string().optional(), // Your thoughts/highlights
  }),
});

// TIL - Things I Learned
const til = defineCollection({
  type: 'content',
  schema: z.object({
    ...commonFields,
    title: z.string(), // Brief title of what you learned
    category: z.string().optional(), // e.g., "JavaScript", "Git", "Design"
  }),
});

// Webmarks - Bookmarks with commentary
const webmarks = defineCollection({
  type: 'content',
  schema: z.object({
    ...commonFields,
    title: z.string(), // Title of the bookmarked content
    url: z.string().url(), // The bookmarked URL
    description: z.string().optional(), // Original description
    // Markdown content will be your notes/commentary
  }),
});

export const collections = {
  posts,
  snippets,
  quotations,
  til,
  webmarks,
};