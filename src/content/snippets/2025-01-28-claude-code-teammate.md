---
title: "Claude Code already have multi-agent orchestration"
createdAt: "2026-01-28T15:32"
draft: false
---

I started reading the original [ChatDev paper](http://arxiv.org/abs/2307.07924) and was chuckling
to myself on how *intentionally* opaque these academic papers can be. They introduce a set based
notation to describe how multiple chat conversations can be strung together. It's honestly a 
waste of brain cells to make such a simple idea so complex.

But, I did like the idea that the multi-agent chats were limited to two agents, instead of 
systems like [AutoGen](https://www.microsoft.com/en-us/research/project/autogen/). 
I haven't yet dived into [claude-flow](https://github.com/ruvnet/claude-flow) yet, but I wondered
whether it was possible to have a claude code agent, or subagent, can spawan a subagent and
have a long running two-way conversation with it.

I asked claude and it said:

> Based on the documentation, the short answer is **no, subagents cannot spawn other subagents**.
> This is an explicit architectural constraint.

I wasn't ready to give up, so I asked:

> is it possible for a claude code agent or subagent to spawn a claude code process as a background
> process and interface with that process in a conversation? thus simulating multi-agent
> (or specifically two agent) topology. and, to search for whether anyone has tried already

And, it turns out that there is [GitHub issue #4182](https://github.com/anthropics/claude-code/issues/4182), 
where users discovered that subagents can work around the "no nested subagents" limitation by shelling out.

But, much more interesting is at the very end of its response it said:

> **The Bottom Line**
>
> Your intuition was correct—spawning claude -p processes is a viable (if hacky) approach that people use today.
> But it turns out Anthropic has already built the proper infrastructure for this (TeammateTool with inbox-based
> messaging between agents), it's just hidden behind feature flags. The community is actively unlocking and
> experimenting with it right now.

***Wait***, what is this `TeammateTool`? A bit of searching, it I found [emergent-minds' post on the topic](https://paddo.dev/blog/claude-code-hidden-swarm/).
It looks like I missed a post on HN while I was in Vegas and it looks like Claude is already working on
productizing multi-agent coding systems. Exciting.
