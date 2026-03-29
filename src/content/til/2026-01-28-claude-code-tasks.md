---
title: "Claude Code Tasks"
url: "https://x.com/trq212/status/2014480496013803643"
createdAt: "2026-01-28T21:52"
draft: false
tags: ["claude"]
---

Last week Claude Code released an update to the Todo tools and now support
[Tasks](https://x.com/trq212/status/2014480496013803643) that are stored in the file system and can be used to corrindate 
across subagents and over multiple sessions. This is very similar to how
I've been using [story/design markdown files with task lists](https://trace.reasoning.net/snippets/2026-01-28-coding-agents-vs-meta-programming/#:~:text=My%20style%20is%20very%20similar%20to%20the%20scoped%20SDLC%20that%20Steve%20Jones%20wrote%20about.%20I%20spent%20most%20of%20my%20interaction%20with%20Claude%20via%20a%20markdown%20file%20that%20we%20edited%20together%20along%20with%20a%20back%20and%20forth%20conversation%20discussing%20the%20design%20document.) 
that are checked in for each feature I'm working on. But, its so much more powerful
and draws inspiration from [beads](https://github.com/steveyegge/beads) by Steve Yegge.

It's crazy to see how Anthropic is so quickly incorporating these features
from the community.

The other wild thing is that Opus is good enough to not even need the Todo
list. This is something we also saw in Thunk throughout 2025. At the end of
2024 a step, which is a Task in a Thunk workflow, would always first start
off by running a "mini-plan" tool. This was akin to a CoT prompt. Experiments
later in the year showed that it didn't seem to create any lift in terms of
accuracy.

What matters more is orchestrating the longer running process now. With
multi-agents and agent swarms this coordination will become more important.
