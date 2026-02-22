# Plan: Add Sveltia CMS Admin UI (Phase 1)

## Context

flatmap.io content is currently created by manually editing Markdown files through the GitHub web UI. Phase 1 of the design doc (`docs/design-blog-creation-ui.md`) calls for adding Sveltia CMS at `/admin/` to provide a rich editing UI with front matter auto-generation, image upload, and markdown editing for all 5 content collections.

The site is fully static (Astro 5.x on GitHub Pages). Sveltia CMS runs entirely in the browser - it loads from a CDN, reads/writes directly to GitHub via API, and needs no server component. Auth is via a GitHub Personal Access Token.

## Files to Create/Modify

| File | Action |
|------|--------|
| `public/admin/index.html` | Create |
| `public/admin/config.yml` | Create |
| `src/content/config.ts` | Modify |

No changes to `astro.config.mjs`, `package.json`, build pipeline, or page templates.

---

## Step 1: Update `src/content/config.ts` — add missing optional fields

Several fields used in actual content files are missing from the Zod schemas. These need to be added so the CMS config accurately maps to the schema and Astro can type-check these fields.

**Changes:**
- **snippets**: Add `title: z.string().optional()` (3 of 4 existing files have a title)
- **til**: Add `title: z.string().optional()` and `url: z.string().optional()` (all 11 files have title, several have url)
- **quotations**: Add `title: z.string().optional()` (2 of 4 files use it)
- Remove the dead `defineConfig` export and unused import at lines 1 and 64-67 (this is not the Astro config file)

Adding `.optional()` fields is backward-compatible — files without these fields continue to work.

## Step 2: Create `public/admin/index.html`

Minimal HTML that loads Sveltia CMS from the unpkg CDN:

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <meta name="robots" content="noindex" />
  <title>Content Manager - flatmap.io</title>
</head>
<body>
  <script src="https://unpkg.com/@sveltia/cms/dist/sveltia-cms.js" type="module"></script>
</body>
</html>
```

No npm dependencies. No SSR. No `astro-sveltia-cms` package (that requires SSR).

## Step 3: Create `public/admin/config.yml`

CMS configuration mapping all 5 collections. Key design decisions:

### Datetime format
Use `format: "YYYY-MM-DDTHH:mm"` — matches the dominant pattern in existing files (e.g., `"2026-01-28T15:32"`). Sveltia will quote the value in YAML, which is consistent with most files. The few unquoted dates in older files are equally valid YAML and `z.coerce.date()` handles both.

### Slug template
All collections use `slug: "{{year}}-{{month}}-{{day}}-{{slug}}"` to match the existing `YYYY-MM-DD-slug.md` naming convention.

### Collections without required titles
- **snippets**: `identifier_field: title` (default). Title is optional but recommended — the user can manually edit the slug if no title is provided.
- **quotations**: `identifier_field: author` — the slug is derived from the author name (e.g., "Scott Alexander" → `scott-alexander`). The user can edit the slug to be more descriptive before saving.
- **til**: Default `identifier_field: title` works since all existing TIL files have titles.

### Tags widget
Uses `list` widget with nested `string` field. Output will be block-style YAML (`tags:\n  - tag1\n  - tag2`) rather than the inline format (`tags: ["tag1", "tag2"]`) in existing files. Both are semantically identical YAML — no functional difference.

### Full collection config

```yaml
backend:
  name: github
  repo: galexy/galexy.github.io
  branch: source

media_folder: public/images
public_folder: /images

slug:
  encoding: ascii
  clean_accents: true
  sanitize_replacement: "-"

collections:
  - name: posts
    label: Posts
    folder: src/content/posts
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    extension: md
    format: yaml-frontmatter
    fields:
      - { label: Title, name: title, widget: string }
      - { label: Description, name: description, widget: string }
      - { label: Created At, name: createdAt, widget: datetime,
          format: "YYYY-MM-DDTHH:mm", date_format: "YYYY-MM-DD", time_format: "HH:mm" }
      - { label: Tags, name: tags, widget: list, default: [],
          field: { label: Tag, name: tag, widget: string } }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: snippets
    label: Snippets
    folder: src/content/snippets
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    identifier_field: title
    summary: "{{createdAt}} - {{title}}"
    extension: md
    format: yaml-frontmatter
    fields:
      - { label: Title, name: title, widget: string, required: false }
      - { label: Created At, name: createdAt, widget: datetime,
          format: "YYYY-MM-DDTHH:mm", date_format: "YYYY-MM-DD", time_format: "HH:mm" }
      - { label: Tags, name: tags, widget: list, default: [],
          field: { label: Tag, name: tag, widget: string } }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: quotations
    label: Quotations
    folder: src/content/quotations
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    identifier_field: author
    summary: "{{createdAt}} - {{author}}"
    extension: md
    format: yaml-frontmatter
    fields:
      - { label: Title, name: title, widget: string, required: false }
      - { label: Author, name: author, widget: string }
      - { label: Source, name: source, widget: string, required: false }
      - { label: URL, name: url, widget: string, required: false }
      - { label: Created At, name: createdAt, widget: datetime,
          format: "YYYY-MM-DDTHH:mm", date_format: "YYYY-MM-DD", time_format: "HH:mm" }
      - { label: Tags, name: tags, widget: list, default: [],
          field: { label: Tag, name: tag, widget: string } }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: til
    label: "Today I Learned"
    folder: src/content/til
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    extension: md
    format: yaml-frontmatter
    fields:
      - { label: Title, name: title, widget: string, required: false }
      - { label: URL, name: url, widget: string, required: false }
      - { label: Created At, name: createdAt, widget: datetime,
          format: "YYYY-MM-DDTHH:mm", date_format: "YYYY-MM-DD", time_format: "HH:mm" }
      - { label: Tags, name: tags, widget: list, default: [],
          field: { label: Tag, name: tag, widget: string } }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }

  - name: webmarks
    label: Webmarks
    folder: src/content/webmarks
    create: true
    slug: "{{year}}-{{month}}-{{day}}-{{slug}}"
    extension: md
    format: yaml-frontmatter
    fields:
      - { label: Title, name: title, widget: string }
      - { label: URL, name: url, widget: string }
      - { label: Created At, name: createdAt, widget: datetime,
          format: "YYYY-MM-DDTHH:mm", date_format: "YYYY-MM-DD", time_format: "HH:mm" }
      - { label: Tags, name: tags, widget: list, default: [],
          field: { label: Tag, name: tag, widget: string } }
      - { label: Draft, name: draft, widget: boolean, default: false }
      - { label: Body, name: body, widget: markdown }
```

## Step 4: Verification

1. **`npm run build`** — confirm schema changes don't break existing content
2. **`npm run dev`** → navigate to `http://localhost:4321/admin/` — confirm CMS loads
3. **Login with PAT** — create a fine-grained token scoped to the repo with Contents: Read and write
4. **Verify all 5 collections load** with existing entries and correct field values
5. **Create a test draft entry** via the CMS — verify correct filename, front matter format, and commit to `source` branch
6. **Verify GitHub Actions triggers** and deploys successfully after CMS commit

## Known Behaviors

- **Editing existing entries** will rewrite front matter to include all fields (e.g., adding `draft: false` and `tags: []` to files that previously omitted them). This is correct but changes file formatting.
- **CMS-created images** will use standard markdown syntax, not the Obsidian-style `|center|400` sizing. Users can manually add sizing syntax in the markdown editor.
- One quotation (`2026-02-20-superintelligence-will-not-help-us-with-everything.md`) has `title:` with an empty value — this parses as `null` in YAML, which `z.string().optional()` will accept.
