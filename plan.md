# Astro Blog Implementation Plan

## Overview

This document outlines the implementation plan for building a multi-content-type blog using Astro with the Terminal theme aesthetic. The blog will support traditional posts alongside snippets, quotations, TIL (Today I Learned) entries, and webmarks, all displayed in a unified timeline view on the homepage.

## Theme Approach

Rather than forking the [Astro Terminal theme](https://github.com/dennisklappe/astro-theme-terminal), we will extract its visual design principles and port them to Tailwind CSS 4. This approach provides several advantages:

- **Maintainability**: We control the entire codebase without upstream theme dependencies
- **Flexibility**: Easier to customize and extend for our multi-content-type needs
- **Modern tooling**: Leverage Tailwind 4's latest features and the Vite plugin integration
- **Clean architecture**: Build exactly what we need without unused theme code

The Terminal theme aesthetic includes:
- Monospace typography (Fira Code font family)
- Retro terminal-inspired design with customizable color schemes
- Syntax highlighting for code blocks
- Clean, minimal layout focused on readability

We'll recreate these design elements using Tailwind's utility classes and custom CSS where needed.

## Technology Stack

**Core Framework**: Astro 5.x with TypeScript (strict mode)

**Styling**: Tailwind CSS 4 via the @tailwindcss/vite plugin, introduced in Astro 5.2. This is the modern, recommended approach that replaces the deprecated @astrojs/tailwind integration. Setup involves adding the Vite plugin to Astro config and importing Tailwind directives in a global CSS file.

**Content Management**: Astro Content Collections with Zod schemas for type safety across all content types

**Deployment**: GitHub Pages with automated deployment via GitHub Actions

## Content Architecture

### Content Collections

We'll define five distinct content collections in `src/content/config.ts`:

1. **posts**: Traditional long-form blog posts with full titles, descriptions, and rich content
2. **snippets**: Short-form content similar to tweets, without formal titles
3. **quotations**: Captured quotes with author attribution and optional source URLs
4. **til**: Today I Learned entries (essentially tagged snippets)
5. **webmarks**: Bookmarked pages with personal notes and commentary

### Common Schema Fields

All content types will share these base fields:
- `createdAt`: Date object for creation timestamp
- `slug`: String for URL generation
- `tags`: Array of strings for categorization
- `draft`: Boolean to control publication status

Each content type will extend these with specific fields (e.g., posts have `title` and `description`, quotations have `author` and `source`).

### Content Type Differentiation

The TIL content type presents an interesting design decision. It could be implemented as:
- A separate collection (our chosen approach for clarity)
- Snippets with a special "til" tag
- Posts with minimal content

We'll implement TIL as its own collection to provide clear separation and allow for type-specific features in the future.

## URL Structure and Routing

### Date-Based URLs for Posts

Posts will use the pattern `/[year]/[month]/[day]/[slug]` implemented through Astro's dynamic routing. We'll create a file at `src/pages/[year]/[month]/[day]/[slug].astro` that uses `getStaticPaths()` to:
- Query the posts collection
- Extract year, month, day from each post's `createdAt` field
- Pad single-digit months/days with leading zeros
- Return params for each post to generate static paths at build time

### Other Content Type URLs

For simplicity and differentiation, other content types will use simpler URL patterns:
- Snippets: `/snippets/[slug]`
- Quotations: `/quotations/[slug]`
- TIL: `/til/[slug]`
- Webmarks: `/webmarks/[slug]`

### Index and Listing Pages

Each content type will have an index page (`/posts`, `/snippets`, etc.) showing all items of that type in reverse chronological order.

## Homepage Timeline

The main index page will display all content types in a unified timeline, grouped by date. Implementation approach:

1. Query all five content collections
2. Combine into a single array with type metadata
3. Sort by `createdAt` descending
4. Group by date (YYYY-MM-DD) using a Map or grouping function
5. Render in collapsible date groups

This provides a stream-of-consciousness view of all blog activity, regardless of content type. Visual differentiation (icons, badges, or subtle styling) will help users identify content types at a glance.

## Phased Implementation

### Phase 3: Vanilla Astro Setup
- Initialize fresh Astro project with TypeScript
- Verify dev server starts successfully
- Test basic routing with Chrome DevTools MCP server
- Commit baseline

### Phase 4: Theming
- Install Tailwind 4 via `npx astro add tailwind`
- Extract Terminal theme color palette and define as Tailwind custom colors
- Configure Fira Code font (local files in `/public/fonts` or Google Fonts)
- Create base layout component with terminal aesthetic
- Implement header, footer, and navigation structure
- Add syntax highlighting configuration for code blocks
- Port Terminal theme CSS to Tailwind utilities

### Phase 5: Posts Timeline
- Define posts collection schema
- Create sample posts with frontmatter
- Implement date-based dynamic route (`/[year]/[month]/[day]/[slug].astro`)
- Build posts listing page (`/posts/index.astro`)
- Create timeline components (TimelineGroup, TimelineItem)
- Update homepage to show posts in timeline format
- Add RSS feed generation

### Phase 6: Snippets Support
- Define snippets collection schema (no title field)
- Create snippet routing and templates
- Add snippets to unified timeline
- Build snippets listing page
- Update RSS feed to include snippets

### Phase 7: Quotations Support
- Define quotations collection schema (author, source, optional URL)
- Create quotation display components with proper attribution
- Add quotations to unified timeline
- Build quotations listing page

### Phase 8: TIL Support
- Define TIL collection schema
- Create TIL routing and templates
- Add TIL to unified timeline
- Build TIL listing page
- Add "Today I Learned" badge or styling

### Phase 9: Webmarks Support
- Define webmarks collection schema (URL, notes, tags)
- Create webmark card component showing link preview
- Add webmarks to unified timeline
- Build webmarks listing page
- Implement external link indicators

## Tag System

All content types support tagging. Implementation includes:
- Tag index page (`/tags`) showing all tags with post counts
- Tag detail pages (`/tags/[tag]`) showing all content with that tag
- Tag clouds or lists on relevant pages

## Technical Considerations

**Type Safety**: Zod schemas provide compile-time type checking for all content, preventing frontmatter errors.

**Performance**: Astro's static generation means all pages are pre-rendered at build time for optimal performance.

**SEO**: Each content type will have proper meta tags, Open Graph data, and inclusion in RSS feeds.

**Accessibility**: Semantic HTML, proper heading hierarchy, ARIA labels where needed, keyboard navigation support.

## Task List

- [ ] Set up vanilla Astro project
- [ ] Install and configure Tailwind 4
- [ ] Create base layout with Terminal theme aesthetic
- [ ] Configure Fira Code typography
- [ ] Define content collection schemas
- [ ] Implement posts collection with date-based routing
- [ ] Build timeline components
- [ ] Create unified homepage timeline
- [ ] Add snippets collection and integration
- [ ] Add quotations collection and integration
- [ ] Add TIL collection and integration
- [ ] Add webmarks collection and integration
- [ ] Implement tag system
- [ ] Configure RSS feed for all content types
- [ ] Set up syntax highlighting
- [ ] Create content type listing pages
- [ ] Test responsive design
- [ ] Verify accessibility
- [ ] Configure deployment to GitHub Pages

## References

- [Astro Terminal Theme](https://astro.build/themes/details/astro-terminal/)
- [Astro Content Collections Docs](https://docs.astro.build/en/guides/content-collections/)
- [Tailwind CSS with Astro](https://docs.astro.build/en/guides/styling/#tailwind)
- [Astro 5.2 - Tailwind 4 Support](https://astro.build/blog/astro-520/)
- [Date-based URLs with Astro](https://www.tomspencer.dev/blog/2023/12/05/date-based-urls-with-astro/)
