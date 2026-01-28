---
title: "To meta-programming with 1000s of agents or not?"
createdAt: "2026-01-28T11:48"
draft: false
---

While working at Thunk.AI, I became pretty comfortable with pair programming with Claude Code, sometimes with up to three agents working
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

I found this, along with well crafted `CLAUDE.md` and skills, to be effective. I even found it to work well with
driving the agent with github issues when I created a [CLI for OCR scanner last week](https://github.com/galexy/docbox).
But, I really want to see if I can build a software "team" with coding agents. So, I'm going to start diving
into meta-programming.

I plan to look at:

- https://github.com/glittercowboy/get-shit-done
- https://github.com/SuperClaude-Org/SuperClaude_Framework
- https://github.com/ruvnet/claude-flow
- https://github.com/wshobson/agents
- https://github.com/steveyegge/gastown
- https://github.com/Yeachan-Heo/oh-my-claudecode
- https://github.com/awslabs/agent-squad
- https://github.com/OpenBMB/ChatDev
- https://github.com/FoundationAgents/MetaGPT

But, while I came up with this list, I came across [embedding-shapes 3 day journey to building a browser](https://emsh.cat/one-human-one-agent-one-browser/) which seems to be the result of the [negative reaction](https://emsh.cat/cursor-implied-success-without-evidence/)
to cursor's post about its [attempt](https://cursor.com/blog/scaling-agents) to try out multi-agent coding.
Cursor's post is pretty vague. Although there are some good pointers, like separating the planner agents 
from the implementors and not overcomplicating the problem with distributed locking protocols, I found some
of the other points rather disappointing. First, I'm surprised they started with "equal" workers and that
they "learned" these agents would not take on larger pieces of work. And, I'm also really surprised they would
only have one PR for a massive refactoring with +266K/-193K edits. That's clearly just vibe coding. Finally,
I'm really disappointed that they didn't share any of their prompts and that the code doesn't even compile.

We're obviously all learning here and the experimention is great. I wonder what ["bitter lesson"](http://www.incompleteideas.net/IncIdeas/BitterLesson.html) 
will be in this space.
