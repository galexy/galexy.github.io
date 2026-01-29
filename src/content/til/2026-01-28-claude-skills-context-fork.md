---
title: "Claude Skills with `context: fork` are like tools defined in natural language"
createdAt: "2026-01-28T18:53"
tags: ["claude", "agents"]
draft: false
---

Claude Skills have an [option](https://code.claude.com/docs/en/skills#run-skills-in-a-subagent)
to run within a subagent and therefore its own context. The option setting `fork` is a bit 
misleading, since the main agent's context is not actually forked. The skill's subagent doesn't
see the parent's context.

The effect of this setting is that the skill description doesn't just become some content
that is added to the context of an agent, it becomes the instructions or the initial user 
prompt for a subagent. 

The documentation also points out that these two features, skills and subagents, 
work bi-directionally. Skills can run in a subagent and subagents can have skills.

When you combine this with being able to [pass arguments to skills](https://code.claude.com/docs/en/skills#pass-arguments-to-skills), 
you essentially have an low barrier for adding new "tools" or "function calls" to the 
underlying LLM. And, these tools are actually defined using a combination of natural
language and hand coded scripts (ahead of time generated code). This is very similar to
the [AI tools](https://info.thunk.ai/en/articles/11691110-ai-tools) in Thunk.AI. However,
it is more powerful, because of the ability to bundle static scripts. In that way, it's
like a combination of Thunk.AI AI tools and code tools. 

Being able to run a skill within its own context helps to prevent the main agent's context
and allows for much more complex and dense instructions plus reference assets that would
consume precious context.

One of the other aspects of this feature that is very well designed, is the ability to
also specify which subagent type to use. This is powerful, because it means that the
subagent that a skill runs in can 1) constrain the tools that are available, 2) take on
a very specific persona and 3) run with a different model that may be more apprioriate
for the task at hand. *We learned the value of #3 multiple times while working with
customers.* For example, we found that GPT-5.2 was great at following SOP type instructions.
While, Gemini was better for extract information from images and documents with its
more robust OCR.

This separation of concerns is a much better design than the AI tools in Thunk.ai. When
defining an AI you also have to essentially configure the subagent (model, tools, etc.).
By separating the concerns, the persona of a subagent can be ***reused*** across multiple
skills.
