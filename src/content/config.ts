import { defineConfig } from 'astro/config';
import { defineCollection, z } from 'astro:content';

const posts = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    description: z.string(),
    createdAt: z.coerce.date(),
    tags: z.array(z.string()).default([]),
    draft: z.boolean().default(false),
  }),
});

const snippets = defineCollection({
  type: 'content',
  schema: z.object({
    createdAt: z.coerce.date(),
    tags: z.array(z.string()).default([]),
    draft: z.boolean().default(false),
  }),
});

const quotations = defineCollection({
  type: 'content',
  schema: z.object({
    author: z.string(),
    source: z.string().optional(),
    url: z.string().optional(),
    createdAt: z.coerce.date(),
    tags: z.array(z.string()).default([]),
    draft: z.boolean().default(false),
  }),
});

const til = defineCollection({
  type: 'content',
  schema: z.object({
    createdAt: z.coerce.date(),
    tags: z.array(z.string()).default([]),
    draft: z.boolean().default(false),
  }),
});

const webmarks = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    url: z.string(),
    createdAt: z.coerce.date(),
    tags: z.array(z.string()).default([]),
    draft: z.boolean().default(false),
  }),
});

export const collections = {
  posts,
  snippets,
  quotations,
  til,
  webmarks,
};

export default defineConfig({
  site: "https://flatmap.io",
  trailingSlash: "always",
});