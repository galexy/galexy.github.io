# Design Document: Blog Post Creation UI for flatmap.io

## Problem Statement

Currently, all content on flatmap.io is created by manually editing Markdown files through the GitHub web UI. This involves:

- Manually writing YAML front matter for each content type (posts, snippets, quotations, TIL, webmarks)
- Uploading images to `public/images/` through separate commits
- No spell checking or writing assistance
- No way to quickly share a quote or webmark from a phone or browser — each requires navigating to GitHub, creating a file, and manually filling in front matter
- No URL shortening for long source URLs
- High friction prevents frequent posting of lightweight content types (webmarks, quotations, TIL)

## Goals

1. **Web-based editing UI** that understands the site's 5 content collections, auto-generates front matter, supports image upload, and provides a markdown editor with spell checking
2. **Quick-capture workflow** from iOS (share sheet) and desktop browsers (extension) that can create webmarks, quotations, and TIL entries in a single action, with content auto-populated from the shared URL or selected text

## Current Architecture Summary

| Aspect | Details |
|--------|---------|
| Framework | Astro 5.x, fully static |
| Content | 5 Zod-validated collections: `posts`, `snippets`, `quotations`, `til`, `webmarks` |
| Content format | Markdown with YAML front matter, stored in `src/content/{collection}/` |
| Naming convention | `YYYY-MM-DD-slug.md` |
| Images | Static files in `public/images/`, Obsidian-style sizing syntax |
| Deployment | GitHub Actions on push to `source` branch → GitHub Pages |
| Custom plugins | `rehype-obsidian-images` (sizing/alignment), `rehype-youtube-embed` |
| Site URL | https://flatmap.io |

### Content Collection Schemas

```
posts:      { title, description, createdAt, tags[], draft }
snippets:   { createdAt, tags[], draft }
quotations: { author, source?, url?, createdAt, tags[], draft }
til:        { title?, url?, createdAt, tags[], draft }
webmarks:   { title, url, createdAt, tags[], draft }
```

---

## Options Evaluated

### Option A: Sveltia CMS (Recommended)

**What:** Git-based headless CMS, successor to Netlify CMS / Decap CMS. Framework-agnostic, open source, free. Currently in beta with v1.0 expected early 2026.

**Pros:**
- Drop-in setup: single HTML page at `/admin/` with a `config.yml`
- Commits directly to the Git repo — no database, no external service to maintain
- Rich media library with drag-and-drop image upload, stock photo integration (Pexels, Pixabay, Unsplash)
- Modern UI built with Svelte (fast, <500KB bundle)
- Configurable content collections map directly to the existing Zod schemas
- Front matter fields auto-generated from collection config (dropdowns, date pickers, tag selectors)
- Markdown editor with live preview
- Mobile-responsive admin UI (works on phones)
- Uses GitHub's GraphQL API for efficient operations
- Auth via GitHub OAuth (Cloudflare Workers free tier) or Personal Access Token
- No changes needed to existing content structure or build pipeline
- Active development with frequent releases

**Cons:**
- Still in beta (though widely used in production)
- OAuth setup requires a small Cloudflare Worker (free tier) since GitHub Pages can't handle OAuth callbacks
- No built-in spell checking (relies on browser's built-in spell check)
- No built-in URL shortening
- No native iOS share sheet — must use browser on mobile

**Auth for GitHub Pages:** Since GitHub Pages is static-only, Sveltia CMS provides [sveltia-cms-auth](https://github.com/sveltia/sveltia-cms-auth), a lightweight Cloudflare Workers script for GitHub OAuth. Alternatively, use a Personal Access Token for developer-only use (simpler, no OAuth needed).

### Option B: Decap CMS

**What:** The original Netlify CMS, now maintained as Decap CMS. Git-based, React-based admin UI.

**Pros:**
- Mature, widely used
- Similar config model to Sveltia
- Large community and documentation

**Cons:**
- Heavier bundle (React-based)
- Slower development pace than Sveltia
- Same OAuth challenge on GitHub Pages
- Sveltia is a strict superset — same config format, more features
- UI feels dated compared to Sveltia

**Verdict:** Sveltia is the better choice since it's API-compatible with Decap but offers a modernized UI and active development. Migration from Sveltia to Decap (or vice versa) is trivial since they share the same config format.

### Option C: TinaCMS

**What:** Git-backed CMS with visual/contextual editing. React-based.

**Pros:**
- Visual editing (live preview in the actual site layout)
- TypeScript-native schema definition
- Official Astro starter template

**Cons:**
- Requires React runtime for editing (adds complexity to Astro project)
- TinaCloud needed for collaboration ($29+/month), or self-hosted Node.js server
- More invasive integration — requires changes to content loading (`tina` schema alongside Astro content collections)
- Heavier setup than Sveltia/Decap
- Less emphasis on Astro in their roadmap

**Verdict:** Over-engineered for a single-author blog. The visual editing is powerful but unnecessary when the primary goal is fast content creation, not WYSIWYG layout editing.

### Option D: StudioCMS

**What:** Astro-native CMS built by the Astro community. SSR-based.

**Pros:**
- Built specifically for Astro
- Plugin system for custom content types
- Astro-native components

**Cons:**
- **Requires SSR** — fundamentally incompatible with the current static/GitHub Pages deployment
- Would require migrating to a server-rendered host (Vercel, Cloudflare, etc.)
- Still in early stages (v0.1.0)
- Database-backed, not git-based — different content model

**Verdict:** Not viable without changing the entire deployment architecture. Interesting for future reference if migrating off GitHub Pages.

### Option E: Custom Micropub Server

**What:** Implement the W3C Micropub protocol — a standardized API for creating/editing content. Connect to existing IndieWeb clients.

**Pros:**
- Standard protocol with ecosystem of existing clients
- iOS apps available: Indigenous (share sheet), Quill, Sparkles
- Browser extensions: Omnibear (bookmarks, replies, likes)
- iOS Shortcuts can call Micropub endpoints
- Perfect for quick-capture of webmarks and quotations
- Full IndieWeb compatibility

**Cons:**
- Requires running a server (Micropub endpoint) that commits to Git
- No existing Astro-specific Micropub server — would need custom development
- Server needs to be hosted somewhere (Cloudflare Workers, Vercel serverless, etc.)
- More complex to build and maintain than adopting an existing CMS
- No built-in rich markdown editor (relies on client apps)

**Verdict:** Excellent for the quick-capture use case (Goal 2) but doesn't address the rich editing UI (Goal 1). Best as a complement to a CMS, not a replacement.

### Option F: iOS Shortcuts + GitHub API

**What:** iOS Shortcuts that call the GitHub Contents API directly to create markdown files.

**Pros:**
- Native iOS share sheet integration
- No server required — calls GitHub API directly
- Can auto-populate front matter from shared URL
- Works with Personal Access Token
- Free, no external dependencies

**Cons:**
- Limited to iOS (no desktop browser extension equivalent)
- Shortcut logic is fragile and hard to maintain
- No markdown editing beyond basic text
- GitHub API has quirks with base64 encoding in Shortcuts
- Each content type needs its own shortcut

**Verdict:** Good lightweight solution for quick capture on iOS. Can complement a CMS.

### Option G: GitHub Actions Workflow Dispatch + iOS Shortcuts

**What:** iOS Shortcut triggers a GitHub Actions workflow via API, passing content as inputs. The workflow creates the markdown file and commits it.

**Pros:**
- Offloads file creation logic to a proper scripting environment (bash/node in Actions)
- iOS Shortcut stays simple (just an API call with inputs)
- Can include validation, slug generation, URL shortening in the workflow
- Desktop equivalent: simple HTTP request from any tool

**Cons:**
- GitHub Actions has a delay (workflow startup ~30 seconds)
- Input fields limited to 10 in workflow_dispatch
- No rich editor

**Verdict:** Better than raw GitHub API for quick capture. Pairs well with a CMS.

---

## Recommended Architecture

A two-pronged approach that addresses both goals:

```
                    ┌──────────────────────┐
                    │   flatmap.io/admin/   │
                    │    (Sveltia CMS)      │
                    │                       │
                    │  - Rich markdown      │
                    │    editor             │
                    │  - Image upload       │
                    │  - Front matter UI    │
                    │  - All 5 collections  │
                    │  - Draft management   │
                    └──────────┬───────────┘
                               │ git commit
                               ▼
┌─────────────────┐    ┌──────────────┐    ┌──────────────────┐
│ iOS Shortcuts    │───▶│  GitHub repo  │───▶│ GitHub Actions   │
│ (share sheet)    │    │  (source)     │    │ (build + deploy) │
│                  │    │              │    │                  │
│ - Webmarks       │    │              │    │ astro build      │
│ - Quotations     │    │              │    │ → GitHub Pages   │
│ - TIL            │    └──────────────┘    └──────────────────┘
└─────────────────┘      ▲
                         │ workflow_dispatch
┌─────────────────┐      │
│ Browser Bookmarklet ───┘
│ or Omnibear ext  │
└─────────────────┘
```

### Layer 1: Sveltia CMS (Goal 1 — Rich Editing UI)

For writing posts, snippets, and any content that benefits from a proper editor.

### Layer 2: GitHub Actions Workflow Dispatch + iOS Shortcuts (Goal 2 — Quick Capture)

For webmarks, quotations, and TIL entries from a phone or browser. An iOS Shortcut captures the URL and selected text from the share sheet, then triggers a GitHub Actions workflow that generates the markdown file with proper front matter and commits it.

### Layer 3 (Future): Micropub Endpoint

If the IndieWeb ecosystem becomes important, a lightweight Micropub server on Cloudflare Workers could bridge Micropub clients to the Git repo. This is optional and can be added later.

---

## Implementation Tasks

### Phase 1: Sveltia CMS Integration

#### Task 1.1: Create the admin page

Create `public/admin/index.html` that loads Sveltia CMS:

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <meta name="robots" content="noindex" />
  <title>Content Manager — flatmap.io</title>
</head>
<body>
  <script src="https://unpkg.com/@sveltia/cms/dist/sveltia-cms.js" type="module"></script>
</body>
</html>
```

#### Task 1.2: Create the CMS configuration

Create `public/admin/config.yml` mapping all 5 content collections:

```yaml
backend:
  name: github
  repo: galexy/galexy.github.io
  branch: source

media_folder: public/images
public_folder: /images

collections:
  - name: posts
    label: Posts
    folder: src/content/posts
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    fields:
      - { label: Title, name: title, widget: string }
      - { label: Description, name: description, widget: string }
      - { label: Created At, name: createdAt, widget: datetime, format: "YYYY-MM-DDTHH:mm" }
      - { label: Tags, name: tags, widget: list, default: [] }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: snippets
    label: Snippets
    folder: src/content/snippets
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    fields:
      - { label: Created At, name: createdAt, widget: datetime, format: "YYYY-MM-DDTHH:mm" }
      - { label: Tags, name: tags, widget: list, default: [] }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: quotations
    label: Quotations
    folder: src/content/quotations
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    fields:
      - { label: Author, name: author, widget: string }
      - { label: Source, name: source, widget: string, required: false }
      - { label: URL, name: url, widget: string, required: false }
      - { label: Created At, name: createdAt, widget: datetime, format: "YYYY-MM-DDTHH:mm" }
      - { label: Tags, name: tags, widget: list, default: [] }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: til
    label: "Today I Learned"
    folder: src/content/til
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    fields:
      - { label: Title, name: title, widget: string, required: false }
      - { label: URL, name: url, widget: string, required: false }
      - { label: Created At, name: createdAt, widget: datetime, format: "YYYY-MM-DDTHH:mm" }
      - { label: Tags, name: tags, widget: list, default: [] }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: webmarks
    label: Webmarks
    folder: src/content/webmarks
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    fields:
      - { label: Title, name: title, widget: string }
      - { label: URL, name: url, widget: string }
      - { label: Created At, name: createdAt, widget: datetime, format: "YYYY-MM-DDTHH:mm" }
      - { label: Tags, name: tags, widget: list, default: [] }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }
```

#### Task 1.3: Set up authentication

**Option A (Simple — PAT):** For single-author use, sign in with a GitHub Personal Access Token. No server-side component needed. Configure in Sveltia CMS settings.

**Option B (Full OAuth):** Deploy [sveltia-cms-auth](https://github.com/sveltia/sveltia-cms-auth) to Cloudflare Workers (free tier):

1. Register a GitHub OAuth App (Settings → Developer Settings → OAuth Apps)
2. Deploy the Cloudflare Worker with the OAuth app credentials
3. Add `base_url` to `config.yml` pointing to the Worker

#### Task 1.4: Test and validate

- Verify all 5 collections render correctly in the admin UI
- Test creating a new post with front matter auto-population
- Test image upload to `public/images/`
- Verify commits land on the `source` branch
- Verify GitHub Actions builds and deploys successfully

### Phase 2: Quick Capture via GitHub Actions + iOS Shortcuts

#### Task 2.1: Create GitHub Actions workflow for content creation

Create `.github/workflows/create-content.yml`:

```yaml
name: Create Content

on:
  workflow_dispatch:
    inputs:
      content_type:
        description: "Content type"
        required: true
        type: choice
        options:
          - webmark
          - quotation
          - til
      title:
        description: "Title (webmarks, optional for TIL)"
        required: false
        type: string
      url:
        description: "Source URL"
        required: false
        type: string
      author:
        description: "Author (quotations only)"
        required: false
        type: string
      source:
        description: "Source name (quotations only)"
        required: false
        type: string
      tags:
        description: "Comma-separated tags"
        required: false
        type: string
      body:
        description: "Content body (markdown)"
        required: true
        type: string

permissions:
  contents: write

jobs:
  create:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v5
        with:
          ref: source

      - name: Create content file
        run: |
          # Generate date components
          DATE=$(date -u +"%Y-%m-%d")
          DATETIME=$(date -u +"%Y-%m-%dT%H:%M")
          YEAR=$(date -u +"%Y")
          MONTH=$(date -u +"%m")
          DAY=$(date -u +"%d")

          # Generate slug from title or body
          TITLE="${{ inputs.title }}"
          BODY="${{ inputs.body }}"
          if [ -n "$TITLE" ]; then
            SLUG=$(echo "$TITLE" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-//' | sed 's/-$//' | cut -c1-50)
          else
            SLUG=$(echo "$BODY" | head -c 50 | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/-/g' | sed 's/--*/-/g' | sed 's/^-//' | sed 's/-$//')
          fi

          TYPE="${{ inputs.content_type }}"
          FILENAME="${DATE}-${SLUG}.md"

          # Build front matter based on content type
          case "$TYPE" in
            webmark)
              FOLDER="src/content/webmarks"
              cat > "${FOLDER}/${FILENAME}" << 'FRONTMATTER'
          ---
          FRONTMATTER
              echo "title: \"${TITLE}\"" >> "${FOLDER}/${FILENAME}"
              echo "url: \"${{ inputs.url }}\"" >> "${FOLDER}/${FILENAME}"
              echo "createdAt: \"${DATETIME}\"" >> "${FOLDER}/${FILENAME}"
              ;;
            quotation)
              FOLDER="src/content/quotations"
              cat > "${FOLDER}/${FILENAME}" << 'FRONTMATTER'
          ---
          FRONTMATTER
              echo "author: \"${{ inputs.author }}\"" >> "${FOLDER}/${FILENAME}"
              [ -n "${{ inputs.source }}" ] && echo "source: \"${{ inputs.source }}\"" >> "${FOLDER}/${FILENAME}"
              [ -n "${{ inputs.url }}" ] && echo "url: \"${{ inputs.url }}\"" >> "${FOLDER}/${FILENAME}"
              echo "createdAt: \"${DATETIME}\"" >> "${FOLDER}/${FILENAME}"
              ;;
            til)
              FOLDER="src/content/til"
              cat > "${FOLDER}/${FILENAME}" << 'FRONTMATTER'
          ---
          FRONTMATTER
              [ -n "$TITLE" ] && echo "title: \"${TITLE}\"" >> "${FOLDER}/${FILENAME}"
              [ -n "${{ inputs.url }}" ] && echo "url: \"${{ inputs.url }}\"" >> "${FOLDER}/${FILENAME}"
              echo "createdAt: \"${DATETIME}\"" >> "${FOLDER}/${FILENAME}"
              ;;
          esac

          # Add tags
          TAGS="${{ inputs.tags }}"
          if [ -n "$TAGS" ]; then
            echo "tags: [$(echo "$TAGS" | sed 's/,/", "/g' | sed 's/^/"/' | sed 's/$/"/' )]" >> "${FOLDER}/${FILENAME}"
          else
            echo "tags: []" >> "${FOLDER}/${FILENAME}"
          fi

          echo "draft: false" >> "${FOLDER}/${FILENAME}"
          echo "---" >> "${FOLDER}/${FILENAME}"
          echo "" >> "${FOLDER}/${FILENAME}"
          echo "$BODY" >> "${FOLDER}/${FILENAME}"

      - name: Commit and push
        run: |
          git config user.name "Content Bot"
          git config user.email "bot@flatmap.io"
          git add src/content/
          git commit -m "Add new ${{ inputs.content_type }}: ${{ inputs.title || 'untitled' }}"
          git push origin source
```

#### Task 2.2: Create iOS Shortcuts

Create three iOS Shortcuts that trigger the workflow:

**Shortcut: "Save Webmark"**
1. Accept share sheet input (URL + optional text)
2. Prompt for title (pre-fill from page title if available)
3. Prompt for tags (optional)
4. Call GitHub API: `POST /repos/galexy/galexy.github.io/actions/workflows/create-content.yml/dispatches`
5. Pass inputs: `content_type=webmark`, `url`, `title`, `body` (selected text or description), `tags`

**Shortcut: "Save Quote"**
1. Accept share sheet input (selected text + URL)
2. Prompt for author name
3. Prompt for source name (optional)
4. Call same GitHub API endpoint
5. Pass inputs: `content_type=quotation`, `body` (selected text), `author`, `source`, `url`

**Shortcut: "Save TIL"**
1. Accept share sheet input or manual text entry
2. Prompt for title (optional)
3. Call same GitHub API endpoint
4. Pass inputs: `content_type=til`, `body`, `title`, `url` (if shared from browser)

Each shortcut uses a GitHub Personal Access Token stored in the Shortcuts app for authentication.

#### Task 2.3: Create browser bookmarklet (desktop quick capture)

A simple bookmarklet that pre-fills a GitHub workflow dispatch form, or opens a lightweight HTML form that calls the GitHub API:

```javascript
javascript:void(window.open(
  'https://github.com/galexy/galexy.github.io/actions/workflows/create-content.yml',
  '_blank'
))
```

Or a more sophisticated bookmarklet that captures the current page title and URL and opens a small form.

### Phase 3: Enhancements (Future)

#### Task 3.1: URL shortening integration

Add a URL shortening step to the GitHub Actions workflow using a service like TinyURL or a custom short domain. This can be applied to webmark URLs and quotation source URLs before committing.

#### Task 3.2: Browser extension (Omnibear or custom)

Evaluate [Omnibear](https://omnibear.com/) for desktop browser quick-capture. If it doesn't support GitHub Actions dispatch, build a lightweight Chrome/Firefox extension that provides a popup form similar to the iOS Shortcuts.

#### Task 3.3: Micropub endpoint

If IndieWeb integration becomes a priority, build a lightweight Micropub endpoint on Cloudflare Workers that:
1. Receives Micropub POST requests
2. Maps them to the appropriate content collection
3. Creates a markdown file with correct front matter
4. Commits to the `source` branch via GitHub API

This would unlock the full ecosystem of Micropub clients (Indigenous for iOS, Quill for web, Omnibear for browser).

#### Task 3.4: Spell checking and writing tools

Sveltia CMS relies on the browser's built-in spell checker. For enhanced writing assistance:
- Use browser extensions like Grammarly or LanguageTool alongside the CMS
- Consider adding a pre-commit hook or GitHub Action that runs a spell checker (e.g., `cspell`) on new content and flags issues

---

## Decision Summary

| Goal | Solution | Effort | Priority |
|------|----------|--------|----------|
| Rich editing UI with front matter, images, drafts | Sveltia CMS | Low | Phase 1 |
| Authentication for CMS | GitHub PAT (simple) or Cloudflare Worker OAuth | Low-Medium | Phase 1 |
| iOS share sheet for webmarks | iOS Shortcut + GitHub Actions workflow | Medium | Phase 2 |
| iOS share sheet for quotations | iOS Shortcut + GitHub Actions workflow | Medium | Phase 2 |
| iOS share sheet for TIL | iOS Shortcut + GitHub Actions workflow | Medium | Phase 2 |
| Desktop quick capture | Browser bookmarklet or extension | Low | Phase 2 |
| Spell checking | Browser built-in + optional Grammarly/LanguageTool | None | Comes free |
| URL shortening | GitHub Actions step with TinyURL API | Low | Phase 3 |
| Micropub/IndieWeb support | Cloudflare Worker Micropub endpoint | High | Phase 3 (optional) |

## Open Questions

1. **PAT vs OAuth:** Is this blog single-author only? If so, PAT is simpler. If collaborators are expected, OAuth is worth the setup.
2. **Existing tags:** Should the CMS show a pre-populated list of existing tags, or allow free-form entry? Sveltia CMS supports both via the `list` widget.
3. **Snippets title field:** The `snippets` collection schema doesn't include `title`, but some existing snippets have a `title` in the front matter. Should this be added to the schema?
4. **TIL title field:** Similarly, `til` has an optional `title` but it's not in the Zod schema — it's mentioned in some posts. Should the schema be updated?
5. **Draft workflow:** Should quick-captured content (from iOS) default to `draft: true` for review, or `draft: false` for immediate publication?

## References

- [Sveltia CMS](https://github.com/sveltia/sveltia-cms) — Git-based headless CMS
- [Sveltia CMS Auth](https://github.com/sveltia/sveltia-cms-auth) — Cloudflare Workers OAuth for GitHub
- [Decap CMS + Astro docs](https://docs.astro.build/en/guides/cms/decap-cms/) — Official Astro integration guide (compatible config format)
- [TinaCMS + Astro docs](https://docs.astro.build/en/guides/cms/tina-cms/) — TinaCMS integration
- [StudioCMS](https://studiocms.dev/) — Astro-native CMS (requires SSR)
- [Micropub spec](https://micropub.spec.indieweb.org/) — W3C standard for content creation
- [Micropub clients](https://indieweb.org/Micropub/Clients) — iOS, web, and browser extension clients
- [GitHub Actions workflow_dispatch](https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows#workflow_dispatch) — Manual workflow triggers
- [iOS Shortcuts + GitHub Actions](https://island94.org/2024/01/trigger-github-actions-workflows-from-apple-shortcuts) — Integration guide
- [Working Copy](https://workingcopy.app/) — Git client for iOS with Shortcuts support
