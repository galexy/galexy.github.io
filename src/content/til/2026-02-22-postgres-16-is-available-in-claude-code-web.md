---
title: Postgres 16 is available in Claude Code Web
url: ''
createdAt: 2026-02-21T22:28
tags:
  - Claude Code
draft: false
---

In my [experiment](https://github.com/galexy/revivo-mk1/) with GSD, I wished that I could use Claude Code web. I wanted to be able to have a long running session working in a remote environment that continued to run even when I closed my laptop. I thought I couldn't do this because I needed a database for running integration and e2e tests.

But, I somehow [missed that the Claude Code for Web documentation](https://code.claude.com/docs/en/claude-code-on-the-web#databases) actually says that Postgres 16 and Redis are installed in the default environment! 

However, the service is not running by default and some tweaking to the configuration is required before local connections can be made. Amazingly enough, Claude Code was able to figure this. I started a remote Claude Code for web session and asked it to change the code and environment to use the installed Postgres instance and repeatedly run the integration tests until they worked. It figured out how to create a [start up hook script](https://github.com/galexy/revivo-mk1/blob/d7b94fe5326bb6275ce01871ce9d6bd98ef10e55/scripts/setup-postgres.sh#L46) that would config the local Postgres instance!

Unfortunately, I don't think running playwright mcp or chrome dev tools mcp is supported. But, this is unlocks for me the ability to run more Claude Code scenarios on the web! Amazing!
