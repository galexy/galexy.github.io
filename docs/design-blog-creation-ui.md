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
2. **Quick-capture workflow** from the browser — a bookmarklet that captures the current page's URL, title, and selected text, opens a lightweight form on the site, and commits directly to GitHub via the Contents API. No GitHub Actions, no leaving the browser.

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

### Option F: Browser Bookmarklets + Capture Page (Recommended for Quick Capture)

**What:** Two bookmarklets in the browser bookmark bar ("Webmark This" and "Quote This") that capture page data and open a lightweight capture form hosted on the site at `/capture/`. The form calls the GitHub Contents API directly to create the markdown file and commit it.

**Pros:**
- Zero friction: see a page, click bookmarklet, tweak form, save — never leave the browser
- Bookmarklets capture `document.title`, `window.location.href`, and `window.getSelection()` automatically
- The capture page is a static HTML file hosted on the site — no server, no external service
- GitHub Contents API creates the file and commits in a single request
- Works on any browser (desktop and mobile)
- Auth via a GitHub fine-grained PAT stored in `localStorage` (same-origin, HTTPS)
- Can share auth with Sveltia CMS if both use the same PAT stored in `localStorage`

**Cons:**
- PAT stored in `localStorage` — acceptable for a personal site over HTTPS, but less secure than OAuth
- Bookmarklets have cross-origin limitations (can't inject into some pages), though this only affects the bookmarklet trigger, not the capture page itself
- No native iOS share sheet integration (would need a separate iOS Shortcuts approach for that)

**Verdict:** Best fit for the quick-capture use case. Purpose-built for the "webmark this page" and "quote this selection" workflow. No intermediaries, no delays, no GitHub Actions in the loop.

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
- Requires navigating to GitHub or building a separate trigger mechanism

**Verdict:** Useful for iOS share sheet integration in the future, but not the right fit for in-browser quick capture. The capture page approach (Option F) is faster and more natural for desktop use.

### Option H: iOS Shortcuts + GitHub API (Future iOS Companion)

**What:** iOS Shortcuts that call the GitHub Contents API directly to create markdown files, triggered from the iOS share sheet.

**Pros:**
- Native iOS share sheet integration
- No server required — calls GitHub API directly
- Can auto-populate front matter from shared URL
- Works with Personal Access Token
- Free, no external dependencies

**Cons:**
- Limited to iOS (no desktop browser equivalent)
- Shortcut logic is fragile and hard to maintain
- No markdown editing beyond basic text
- GitHub API has quirks with base64 encoding in Shortcuts
- Each content type needs its own shortcut

**Verdict:** Good future companion for mobile capture. Can complement the capture page for when you're on your phone and want to share from Safari or another app.

---

## Recommended Architecture

A two-pronged approach that addresses both goals:

```
┌─────────────────────────────────────────────────────────────────┐
│                        flatmap.io                               │
│                                                                 │
│  /admin/  (Sveltia CMS)         /capture/  (Quick Capture)     │
│  ┌───────────────────────┐      ┌───────────────────────┐      │
│  │ Rich markdown editor  │      │ Lightweight form       │      │
│  │ Image upload          │      │ Pre-filled by          │      │
│  │ Front matter UI       │      │   bookmarklet          │      │
│  │ All 5 collections     │      │ Webmarks + Quotations  │      │
│  │ Draft management      │      │ Tag entry              │      │
│  └──────────┬────────────┘      └──────────┬────────────┘      │
│             │                              │                    │
└─────────────┼──────────────────────────────┼────────────────────┘
              │ git commit                   │ GitHub Contents API
              │ (GitHub GraphQL API)         │ (create file + commit)
              ▼                              ▼
       ┌──────────────┐              ┌──────────────┐
       │  GitHub repo  │              │  GitHub repo  │
       │  (source)     │──────────────│  (source)     │
       └──────┬───────┘              └──────────────┘
              │ push triggers
              ▼
       ┌──────────────────┐
       │ GitHub Actions    │
       │ astro build       │
       │ → GitHub Pages    │
       └──────────────────┘

  Browser bookmarklets:
  ┌──────────────────────────────────────────────┐
  │ [Webmark This]  captures URL + title         │
  │ [Quote This]    captures selection + URL      │──→ opens /capture/
  │                 + title                       │    with query params
  └──────────────────────────────────────────────┘
```

### Layer 1: Sveltia CMS (Goal 1 — Rich Editing UI)

For writing posts, snippets, and any content that benefits from a proper editor. Sveltia CMS provides a full admin UI at `/admin/` with markdown editing, image upload, front matter management, and draft workflows for all 5 content collections.

### Layer 2: Capture Page + Bookmarklets (Goal 2 — Quick Capture)

For webmarks and quotations captured while browsing. Two bookmarklets sit in the bookmark bar:

- **"Webmark This"** — grabs `document.title` and `window.location.href`, opens `/capture/?mode=webmark&url=...&title=...`
- **"Quote This"** — grabs `window.getSelection().toString()`, the URL, and the title, opens `/capture/?mode=quotation&text=...&url=...&title=...`

The capture page at `/capture/` is a static HTML page with a form pre-filled from URL parameters. On submit, it calls the GitHub Contents API (`PUT /repos/:owner/:repo/contents/:path`) to create the markdown file with proper front matter and commit it directly to the `source` branch. Auth is via a GitHub fine-grained PAT stored in `localStorage`.

### Layer 3 (Future): iOS Shortcuts

For mobile capture from the iOS share sheet. Shortcuts call the GitHub Contents API directly with a PAT, using the same file creation logic as the capture page. This extends the quick-capture workflow to mobile without requiring any server infrastructure.

### Layer 4 (Future): Micropub Endpoint

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

### Phase 2: Quick Capture via Bookmarklets + Capture Page

#### Task 2.1: Create the capture page

Create `public/capture/index.html` — a static HTML page with a form for quick content capture. This page is separate from Sveltia CMS and purpose-built for speed.

**Capture page requirements:**

- Two modes: **Webmark** and **Quotation**, selectable via tabs and auto-selected from URL parameters
- Pre-filled from URL query parameters set by the bookmarklets
- Form fields per mode:
  - **Webmark**: title, url, tags (comma-separated), body (optional description/commentary)
  - **Quotation**: author, source (optional), url (optional), tags, body (the quote text)
- `createdAt` auto-set to current datetime
- `draft` defaults to `false`
- Slug auto-generated from title (webmarks) or first ~50 chars of body (quotations)
- Filename follows `YYYY-MM-DD-slug.md` convention
- On submit: calls GitHub Contents API to create the file and commit

**GitHub Contents API call:**

```
PUT /repos/galexy/galexy.github.io/contents/src/content/{collection}/{filename}
Authorization: token {PAT}
Content-Type: application/json

{
  "message": "Add webmark: {title}",
  "content": "{base64-encoded markdown with front matter}",
  "branch": "source"
}
```

**Capture page UI:**

- Clean, minimal design — optimized for speed, not browsing
- Auto-focuses the first editable field
- Shows a success message with a link to the created file on GitHub after saving
- Settings panel (collapsed by default) for entering/updating the GitHub PAT
- PAT stored in `localStorage` under a key like `flatmap-github-pat`
- First-time setup: if no PAT found, shows a setup prompt with instructions for creating a fine-grained PAT

#### Task 2.2: Create browser bookmarklets

Two bookmarklets for the browser bookmark bar:

**"Webmark This" bookmarklet:**

```javascript
javascript:void(open(
  'https://flatmap.io/capture/?mode=webmark'
  + '&url=' + encodeURIComponent(location.href)
  + '&title=' + encodeURIComponent(document.title),
  '_blank'
))
```

**"Quote This" bookmarklet:**

```javascript
javascript:void(open(
  'https://flatmap.io/capture/?mode=quotation'
  + '&url=' + encodeURIComponent(location.href)
  + '&title=' + encodeURIComponent(document.title)
  + '&text=' + encodeURIComponent(getSelection().toString()),
  '_blank'
))
```

**User workflow:**

1. Browse any webpage
2. (For quotations) Highlight the text you want to quote
3. Click the bookmarklet in the bookmark bar
4. The capture page opens in a new tab with URL, title, and selected text pre-filled
5. Add tags, tweak the title, fill in the author (for quotations), add commentary
6. Click **Save**
7. The file is committed to the `source` branch → GitHub Actions rebuilds → live on the site

#### Task 2.3: Implement PAT authentication

**GitHub fine-grained Personal Access Token setup:**

1. Go to GitHub → Settings → Developer Settings → Personal Access Tokens → Fine-grained tokens
2. Create a token with:
   - **Repository access**: Only `galexy/galexy.github.io`
   - **Permissions**: Contents → Read and write (minimum required)
   - **Expiration**: 1 year (or custom)
3. Paste the token into the capture page's settings panel
4. Token stored in `localStorage` (`flatmap-github-pat`)
5. All GitHub API calls from both the capture page and (optionally) Sveltia CMS use this token

**Security considerations:**
- Fine-grained PAT is scoped to a single repo with minimal permissions
- `localStorage` is same-origin only — only pages on `flatmap.io` can read it
- Site is served over HTTPS (GitHub Pages enforces this)
- Token can be revoked instantly from GitHub settings
- Acceptable risk profile for a single-author personal blog

#### Task 2.4: Test and validate

- Test "Webmark This" bookmarklet from various sites (news, Wikipedia, GitHub, YouTube)
- Test "Quote This" bookmarklet with text selected on a page
- Verify files are created with correct front matter and naming convention
- Verify the commit lands on the `source` branch
- Verify GitHub Actions builds and deploys successfully
- Test PAT setup flow on first visit
- Test error handling (invalid PAT, network failure, duplicate filename)

### Phase 3: Enhancements (Future)

#### Task 3.1: iOS Shortcuts for mobile capture

Create iOS Shortcuts that call the GitHub Contents API directly for share sheet integration:

- **"Save Webmark"** shortcut — accepts a URL from the share sheet, prompts for tags, creates the markdown file via GitHub API
- **"Save Quote"** shortcut — accepts selected text + URL, prompts for author, creates the file

Uses the same PAT-based auth and file creation logic as the capture page.

#### Task 3.2: URL shortening integration

Add an optional URL shortening step to the capture page using a service like TinyURL or a custom short domain. This can be applied to webmark URLs and quotation source URLs before committing.

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
| Rich editing UI with front matter, images, drafts | Sveltia CMS at `/admin/` | Low | Phase 1 |
| Authentication | GitHub fine-grained PAT in `localStorage` | Low | Phase 1 |
| Desktop quick capture — webmarks | Bookmarklet + capture page at `/capture/` + GitHub Contents API | Medium | Phase 2 |
| Desktop quick capture — quotations | Bookmarklet + capture page at `/capture/` + GitHub Contents API | Medium | Phase 2 |
| Spell checking | Browser built-in + optional Grammarly/LanguageTool | None | Comes free |
| iOS share sheet capture | iOS Shortcuts + GitHub Contents API | Medium | Phase 3 |
| URL shortening | Capture page integration with TinyURL API | Low | Phase 3 |
| Micropub/IndieWeb support | Cloudflare Worker Micropub endpoint | High | Phase 3 (optional) |

## Open Questions

1. **Existing tags:** Should the CMS and capture page show a pre-populated list of existing tags, or allow free-form entry? Sveltia CMS supports both via the `list` widget. The capture page could fetch existing tags from the repo for autocomplete.
2. **Snippets title field:** The `snippets` collection schema doesn't include `title`, but some existing snippets have a `title` in the front matter. Should this be added to the schema?
3. **TIL title field:** Similarly, `til` has an optional `title` but it's not in the Zod schema — it's mentioned in some posts. Should the schema be updated?
4. **Draft workflow:** Should quick-captured content default to `draft: true` for review, or `draft: false` for immediate publication?
5. **TIL in capture page:** Should the capture page support TIL entries in addition to webmarks and quotations? TIL entries have a simpler schema (just body + optional title/url) and could benefit from quick capture.

## Decisions Made

1. **PAT over OAuth:** This is a single-author blog. A GitHub fine-grained PAT scoped to the repo with `Contents: Read and write` is simpler and sufficient. OAuth can be added later if needed.
2. **Capture page over GitHub Actions:** Quick capture happens entirely in the browser via the GitHub Contents API. No GitHub Actions workflow dispatch, no delays, no leaving the browser.
3. **Bookmarklets over browser extension:** Bookmarklets work in any browser with zero installation. A Chrome/Firefox extension would offer richer integration (e.g., context menu "Quote this") but is more effort to build and maintain.

## References

- [Sveltia CMS](https://github.com/sveltia/sveltia-cms) — Git-based headless CMS
- [Sveltia CMS Auth](https://github.com/sveltia/sveltia-cms-auth) — Cloudflare Workers OAuth for GitHub
- [Decap CMS + Astro docs](https://docs.astro.build/en/guides/cms/decap-cms/) — Official Astro integration guide (compatible config format)
- [TinaCMS + Astro docs](https://docs.astro.build/en/guides/cms/tina-cms/) — TinaCMS integration
- [StudioCMS](https://studiocms.dev/) — Astro-native CMS (requires SSR)
- [GitHub Contents API](https://docs.github.com/en/rest/repos/contents#create-or-update-file-contents) — Create files via REST API
- [GitHub Fine-grained PATs](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#fine-grained-personal-access-tokens) — Scoped token creation
- [Micropub spec](https://micropub.spec.indieweb.org/) — W3C standard for content creation
- [Micropub clients](https://indieweb.org/Micropub/Clients) — iOS, web, and browser extension clients
- [Working Copy](https://workingcopy.app/) — Git client for iOS with Shortcuts support
