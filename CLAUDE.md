# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Personal blog for Galex Yen at https://trace.reasoning.net, built with Astro 5, Tailwind CSS 4, and TypeScript (strict mode). Deployed to GitHub Pages from the `source` branch via GitHub Actions.

## Commands

- `npm run dev` — Start dev server
- `npm run build` — Type-check and build (`astro check && astro build`)
- `npm run preview` — Preview production build locally

## Architecture

### Content System

Five content collections defined in `src/content/config.ts` with Zod schemas:

| Collection | Key Fields | URL Pattern |
|---|---|---|
| **posts** | title, description, createdAt, tags?, draft? | `/YYYY/MM/DD/slug` |
| **snippets** | createdAt, title? (nullable), tags?, draft? | `/snippets/slug` |
| **quotations** | author, createdAt, source?, url?, tags?, draft? | `/quotations/slug` |
| **til** | createdAt, title?, tags?, draft? | `/til/slug` |
| **webmarks** | title, url, createdAt, tags?, draft? | `/webmarks/slug` |

Content files use the naming convention `YYYY-MM-DD-slug.md`. All collections filter out `draft: true` items.

### Routing

File-based routing in `src/pages/`. The homepage (`index.astro`) renders a unified timeline of all content types grouped by date. Date-based archive pages exist at `/[year]`, `/[year]/[month]`, `/[year]/[month]/[day]`. Tags are cross-content-type at `/tags/[tag]`.

Two RSS feeds: `/rss.xml` (all content) and `/posts/rss.xml` (posts only).

### Utility Libraries

Each content type has a corresponding `src/lib/*.ts` module that provides URL generation, date parsing, and preview text extraction.

### Custom Markdown Plugins

- `src/plugins/rehype-youtube-embed.mjs` — Converts YouTube URLs in image syntax to responsive iframes (supports regular videos and Shorts with 9:16 aspect ratio)
- `src/plugins/rehype-obsidian-images.mjs` — Supports Obsidian-style image sizing/alignment: `![Alt|WIDTHxHEIGHT|ALIGN](url)`

Markdown config in `astro.config.mjs` also includes remark-math, rehype-katex, and remark-figure-caption.

### Styling

Terminal-inspired theme using Tailwind CSS 4 (via `@tailwindcss/vite` plugin). Custom properties and styles in `src/styles/global.css`. Font: Fira Code (monospace). Code blocks get language labels and copy buttons via `src/scripts/code-blocks.js` (client-side).

### Components

- `BaseLayout.astro` — Main HTML wrapper (includes GTM, meta tags, Open Graph)
- `TimelineGroup.astro` / `TimelineItem.astro` — Homepage timeline rendering
- `Comments.astro` — Disqus integration

### Environment

- `PUBLIC_GTM_ID` — Google Tag Manager ID (set as GitHub Actions secret for builds)
