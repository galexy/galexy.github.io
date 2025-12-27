---
title: "Tailwind CSS 4: What's New"
description: "Exploring the new features in Tailwind CSS version 4"
createdAt: 2025-12-24
tags: ["css", "tailwind", "webdev"]
draft: false
---

Tailwind CSS 4 brings some exciting changes to the popular utility-first CSS framework.

## Major Changes

### New Vite Plugin

The biggest change is the new `@tailwindcss/vite` plugin which replaces the old PostCSS integration:

```javascript
import tailwindcss from '@tailwindcss/vite';

export default {
  plugins: [tailwindcss()],
};
```

### Simplified Configuration

No more `tailwind.config.js` in many cases! You can now use CSS variables and the `@theme` directive:

```css
@theme {
  --color-primary: #eec35e;
  --font-mono: "Fira Code", monospace;
}
```

## Benefits

- **Faster builds** - The Vite plugin is significantly faster
- **Simpler setup** - Less configuration needed
- **Better DX** - Improved developer experience

## Migration

Migrating from Tailwind 3 is straightforward:

1. Install `@tailwindcss/vite`
2. Update your Vite config
3. Convert config to `@theme` blocks
4. Test thoroughly

Overall, Tailwind 4 is a solid upgrade that makes the framework even better!
