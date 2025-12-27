---
createdAt: 2025-12-26
tags: ["javascript", "tips"]
draft: false
---

Quick way to deduplicate an array in JavaScript using Set:

```javascript
const unique = [...new Set(array)];
```

Works with primitives, but remember that objects are compared by reference!
