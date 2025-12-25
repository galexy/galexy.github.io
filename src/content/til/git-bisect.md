---
createdAt: 2025-12-22T16:00:00Z
slug: 'git-bisect-debugging'
title: 'Using git bisect to find bugs'
category: 'Git'
tags: ['git', 'debugging']
---

You can use `git bisect` to binary search through your commit history to find which commit introduced a bug.

```bash
git bisect start
git bisect bad  # Current version is bad
git bisect good v1.0  # v1.0 was good
# Git will checkout commits for you to test
git bisect good/bad  # After each test
git bisect reset  # When done
```

This is way faster than manually checking commits!
