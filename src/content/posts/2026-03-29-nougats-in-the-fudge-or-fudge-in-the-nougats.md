---
title: Nougats in the fudge or Fudge in the nougats
description: Determinism in a non-deterministic world
createdAt: 2026-03-28T18:19
tags:
  - llms
  - code-gen
draft: false
---

In Thunk.ai, agent loops are executed within the steps of a workflow. Thunk is the workflow orchestrator written in code - which is the deterministic part of the system. And, the agent loops running within a step of the workflow was the non-deterministic part.

Tony Fader came up with a great way to one visualize this mixing of deterministic code and with non-deterministic LLMs. Thunk steps was like a nougat (code) with fudge (LLM) in the middle. It's the "hard" shell with the "soft" inside.  But, we also had nougats in the fudge. Those were the tools that the agent could call on each iteration of the loop.

Thunk's programming model was originally all natural language. This is great for transitioning from human-driven processes to AI-driven process because LLMs can deal with variability in the inputs in ways that deterministic code could not. 

But over time we realized that the LLM's non-determinism was not only unreliable, it a wasteful use of tokens. Tools can serve the purpose of introducing code that the agent can call, but that tool code needs to be available to the agent loop. Thunk wanted to lower the bar for LLM development and asking users to write code didn't seem like a good idea. After a few experiments, Tony came up with the idea of asking the LLM to inspect the instructions and determine if there were any parts of the instructions  that could be made deterministic and it would generate javascript that would dynamically be made available as a tool to the LLM agent. Tony found a way to put a nougat in the fudge.

Earlier this month, I came across a project that solved the opposite problem of putting the fudge in the nougat and they called them [AI functions](https://blog.mikegchambers.com/posts/software-31-ai-functions/). You create python function stubs that have LLM descriptions of the function in comments. (Huh... kinda like copilot v1. Remember that, when we prompted copilots by writing functions. What were we thinking?) At first, I thought this was a neat idea.

But, then I realized that Claude Code really simplifies these problems. It generates code on demand if needed and calls it with the \`bash\` tool. It calls existing coding, like a CLI, using the \`bash\` tool. It can be called itself using the \`-p\` option via its CLI or programmatically via the Agent SDK. Claude Code is the fudge that can make the nougats on demand.
