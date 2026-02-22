---
title: e2b.dev for running code in sandboxes
url: https://e2b.dev
createdAt: 2026-02-21T22:08
tags:
  - microVM
  - sandbox
  - untrusted code
  - Claude Code
draft: false
---

I've been researching and thinking about how to create isolated environments for my Claude Code workflow. For my experiments with [GSD](https://github.com/gsd-build/get-shit-done) I working entirely in a devcontainer based off the Claude Code [reference container](https://github.com/anthropics/claude-code/tree/main/.devcontainer). I updated it to use docker compose to host Postgres. But, I felt like I was missing out on being able to use [Claude Code Web](https://code.claude.com/docs/en/claude-code-on-the-web). 

I started looking around at what others were doing and found https://github.com/disler/agent-sandbox-skill and the disler/indydevdan's [YouTube channel](https://www.youtube.com/@indydevdan). Honestly, this guy's videos are super hard to follow. He just jumps right into something, with almost zero context (like usually that the demos he's doing is really some open source code he wrote). And, I feel he intentionally talks real fast just to impress you with a ton of things happening hoping to impress you with these "sheet of your pants" examples.

Anyway, in several videos he calms to "scale his impact by scaling his agents". It turned out what he has done is create scripts that are wrapped by claude commands that spin up sandboxes and instead of having Claude Code call bash commands on the local machine, sends commands to these sandboxes. At first, this seems like a great idea, but then he says in one video that this allows him to try different ideas and when he likes the outcome of one, he "copies" the code down. This seems worse than hacky.

However, the sandbox solution he uses is pretty cool. It's [E2B](https://e2b.dev) and seems to make it very easy to spin up a microVM (Firecracker) and run some untrusted code. The API is easy and definitely great for cases where I'll be generated code on the fly and executing it. It's very similar to fly.io's sprites. But, seems to be more popular.
