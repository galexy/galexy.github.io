---
title: LiteLLM supply chain attack
createdAt: 2026-03-24T12:22
tags:
  - security
draft: false
---

See https://github.com/BerriAI/litellm/issues/24512. 

> The `litellm==1.82.8` wheel package on PyPI contains a malicious `.pth` file (`litellm_init.pth`, 34,628 bytes) that **automatically executes a credential-stealing script every time the Python interpreter starts** — no `import litellm`required.

How the hell?
