---
title: "To meta-programming with 1000s of agents or not?"
createdAt: "2026-01-28T11:48"
draft: true
---

I became pretty comfortable with pair programming with Claude Code, sometimes with up to three agents working
on completely separate features at the same time. My style is very similar to the scoped SDLC that Steve Jones
[wrote](https://blog.metamirror.io/turning-claude-code-into-a-delegate-not-a-vibe-a-kick-starter-d646e73a8f0c) 
about. I spent most of my interaction with Claude via a markdown file that we edited together along with
a back and forth conversation discussing the design document. Most of the features were large enough that even
a human would have a hard time doing the work in a single PR. So, a key result of the design stage was coming up
with a list of major phases of the work, each of which resulted in a digestable PR. At the end of each phase,
the design document was updated based on the actual changes that were made and learnings from mistakes the agent
made. This was helpful for some  of the larger features that would easily go sideways if Claude didn't have a 
document to reference. And, it also made clearing the context easier, because I would ask it to re-read the
document.

