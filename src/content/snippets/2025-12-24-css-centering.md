---
createdAt: 2025-12-24
tags: ["css", "layout"]
draft: false
---

The modern way to center anything with CSS:

```css
.container {
  display: grid;
  place-items: center;
}
```

No more margin tricks or flexbox incantations. `place-items` is shorthand for `align-items` + `justify-items`.
