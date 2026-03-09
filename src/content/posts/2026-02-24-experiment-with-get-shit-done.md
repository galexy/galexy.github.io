---
title: Experiment with Get Shit Done
description: My experience with GSD coding agent framework
createdAt: 2026-03-08T17:21
tags:
  - Claude Code
  - coding-agent
draft: false
---

At the start of the year, I realized that engineering coding agents is becoming more important than programming
the LLMs directly. I wanted to figure out the **fundamental primitives** of this new programming environment. But,
rather than working bottom up, I decided to evaluate some of the more _complete_ frameworks — like
[Get Shit Done](https://github.com/gsd-build/get-shit-done), SuperClaude, and claude-flow — to learn by
example how they leveraged Claude Code.

My goal isn't to find a framework that I would adopt. I suspect learning these frameworks is akin to learning
[jQuery](https://jquery.com) in the early days of Web 2.0. Useful, because you get high-level reps. But,
ultimately learning the fundamentals of javascript and keeping up with the spec as it evolved was more
important. I want to form a mental model of what the primitives for coding agents could be.

In this post, I'll describe how I decided to evaluate frameworks, walk through how GSD works, summarize my
impression and finish with what I learned about _"coding"_ coding agents.

# Defining the experiment

I wanted a consistent way to evaluate the potential frameworks I might test. So, I chose to build a very stripped down
version of a Quicken clone with a python backend and a react frontend. I didn't need to completely build the
product. But, I wanted the framework to have the context that I was building a medium size production application. I
would start from a greenfield repo, no other `CLAUDE.md` files or skills. My goal was to form a **_subjective_**
opinion on how effective the framework was at helping me build the product. I wanted to know if I could rapidly develop
products focusing on describing features, requirements and what good looks like. I also wanted to know if the framework
would enable me to scale development to dozens or even hundreds of coding agents working in parallel.

## Some Constraints

- No manually written code
- Try to only provide requirements and constraints in whatever form the framework prefers

The first framework I would test was [Get Shit Done](https://github.com/gsd-build/get-shit-done). Let's first go over
the GSD workflow and its user experience. I'll start by walking you through GSD's workflow. Then I'll detail my
impression of the framework and my key findings.

I also spent some time looking at the framework's code. I've detailed some notes and learnings in the appendix.

# How does GSD work?

GSD is a set of custom Claude Code commands that you install via node.js. It delegates heavy
lifting to [subagents](https://platform.claude.com/docs/en/agent-sdk/subagents) for
[context isolation](https://platform.claude.com/docs/en/agent-sdk/subagents#context-isolation),
keeping your interactive session's context window clean.

## Describe your project with `/gsd:new-project`

You start by running `/gsd:new-project` to describe the program you want to build. The workflow
identifies "gray areas" in your proposal and asks you a series of questions (four at a time) to
clarify your ideas. For each question, the agent gives you options to choose from or you enter a
short free form answer. It's like a **choose your own adventure** style of product development.
Investing time to answer more questions front-loads decisions.

![Running `gsd:new-project`](/videos/gsd-new-project.mp4)

After scoping, the workflow kicks off subagents to research the domain and tech stack, then
proposes a series of **phases** as part of **milestone** 1. All artifacts are stored as markdown
files in a `.planning` folder.

Each phase follows a four-step cycle. Between each step, you call `/clear` to reset the context
window.

1. **Discuss** (`/gsd:discuss-phase`) — Same Q&A pattern as project setup, but focused on a
   single phase. You pick topics (requirements, technical design, domain) and answer questions
   to flesh out details. Output: `CONTEXT.md`.

![Running /gsd:discuss-phase](/videos/gsd-discuss.mp4)

2. **Plan** (`/gsd:plan-phase`) — Spawns a research subagent to study the codebase and web for
   relevant patterns, then a planning subagent that generates individual plan files with objectives
   and tasks. Tasks can be `auto` (self-contained), `tdd`, or `checkpoint:human-verify`.

3. **Execute** (`/gsd:execute-phase`) — Spawns parallel subagents for each plan within a wave,
   working through waves sequentially. Each task is committed as it completes. Output: `SUMMARY.md`
   per plan, recording accomplishments, decisions, deviations, and issues.

4. **Verify** (`/gsd:verify-phase`) — Generates UAT tests from the summaries and walks you through
   them one by one. You report pass/fail; issues are recorded in `UAT.md`. Gaps can theoretically
   be addressed by re-planning or inserting a new phase, though the documentation wasn't clear on
   this and I never found a natural way to do it.

Then you start the cycle again with the next phase.

# Evaluating GSD

You can see the full result of the experiment that I ran in my [`revivo-mk1`](https://github.com/galexy/revivo-mk1/)
repo. I went through 20 phases. There is both a backend and frontend, with some basic parts of the personal finance
domain implemented for the backend. The front-end has auth and doesn't quite have transaction editing working. Repeating
the workflow twenty times was more than enough for me to form an opinion.

Here are my key findings.

## No backpressure on the coding agent

For the first half dozen phases, the code that was generated failed 100% of the time when we got to the verification
phase. The execution phase was not instructed to do things to verify the code would actually work. It was instructed
to execute the plans. It wasn't instructed to verify that the code compiled or whether the tests even passed. There
was no backpressure on the coding agent. The agent would generate unit tests that passed or integration tests that
mostly passed. But, it never ran the code e2e to prove to itself that it worked. And, it would often notice issues and
merely record them in the `SUMMARY.md` file.

Without this backpressure, I consistently saw the coding agent make all the mistakes that expert vibe coders warn
you about. These included the issues that Gene Kim and Steve Yegge warn about in their book,
[_Vibe Coding_](https://www.amazon.com/Vibe-Coding-Building-Production-Grade-Software/dp/1966280025). This
is an excellent book by the way. Buy it (I'm not an Amazon affiliate - yet). The issues include:

- **Reward hijacking** — or what Yegge calls the "Cardboard Muffin" problem. It doesn't actually complete the work
  it set out to do, but claims completion anyway.
- **Working around blocking issues** — In the first phase which required it to update the devcontainer, it just
  walked right past the fact that the dependencies it added were not yet installed and said "as expected". It had no
  way of testing that the changes it made were in fact working, but claimed success. In a later phase, it hit an
  issue, claimed that it was already there, so it wasn't its fault and proceeded to implement a nonsense workaround.
- **Relying on the user for testing** — The execution of a phase didn't have enough mechanisms to push verification
  earlier in the process, but instead expects verification at the very end via `/gsd:verify-phase`. I got fed up with
  manually verifying the generated code and instead started asking it to automate the verification steps by running
  curl commands itself and opening a web browser using the
  [chrome devtools mcp](https://github.com/ChromeDevTools/chrome-devtools-mcp).

There was supposedly TDD support, but I had a hard time triggering this in the planning phases. And TDD isn't the only
way to shift verification left. Compiler errors and warnings, linting errors, formatting errors, actual e2e testing
(asking the agent to prove the code works) — these are all ways to reduce the manual verification burden. It is possible
that I could have asked for the plans to do more verification upfront. But, I think that should be baked into the
framework and not require that I remember to nudge it every phase that gets planned.

> Oddly, the verify command is supposed to spawn debugging agents for each issue found, but I never saw it
> actually do this.

## Plans lacked conceptual depth

I like the brainstorming quality of the Q&A in both `/gsd:new-project` and `/gsd:discuss-phase`. If you kept having
it ask you questions, it would come up with a bunch of things that you wouldn't have thought of right away. However,
I didn't really feel like I was developing requirements and thinking through what the features really were. The outputs
were bullet points, short statements — not prose with clear topic sentences that explained _why_ the features needed to
be the way they were. The workflow nudges you to the next step too quickly.

The plans themselves had a deeper problem. If you consider phases to be epics and plans to be stories, you need to
describe the intent of the story at a _conceptual_ level, separate from the _operational_ details of implementation.
This separation of framing a problem into conceptual language from operational details is one of the most important
skills I learned as a senior engineer in my final years at Microsoft. When working in a domain like transactions for
personal finance, there are two problems that need to be solved — what is the conceptual model that addresses the
overall product feature? And, how do we best implement this conceptual model at this point in time?

An example of this separation can be seen in every relational database. _Tables_, _Indexes_, etc. are concepts modeled
to communicate to the user. Under the hood, these are implemented in a variety of different ways that may or may not
closely resemble what the user sees. Both views need to be explored. In the plans that were generated, **too many
tokens were wasted** on generating what code would have been written. These plans are _detailed_ operational step by
step plans — but they never forced thinking about the domain model itself.

I learned this at [phase 3 of my experiment](https://github.com/galexy/revivo-mk1/tree/main/.planning/phases/03-transaction-domain)
where we worked on the transaction domain. What you see in the repo is actually my second attempt at phase 3. The
initial plan didn't account for the nuances of how splitting a transaction into multiple categories would interact with
transfer transactions. I didn't look at the plan closely enough, and when execution was complete, the domain model was
too simple. I restarted phase 3 and spent much more time discussing the complexity of the domain.

This was partly my fault for not reviewing the plans in detail. But the workflow made it hard:

- There is no built-in feedback mechanism for reviewing plans. You have to read them yourself and force a conversation
  to update them.
- Too many plans are generated all at once, making it difficult to collate feedback. I would have preferred to review
  the proposed plans in a PR _before_ they were committed, riddling it with comments on what needed to change.
- The discussion phase didn't force a detailed set of acceptance criteria. The discussion focused on the _requirements_
  and not on how to _verify_ that those requirements would be met.

## Too linear for real development

The GSD process is very linear and waterfall-like. It expects you to work through phases sequentially. If you change
your mind about the order of phases, you need to run a command to update the roadmap, plans, etc. I also found that
addressing issues was not natural. I didn't know if I was supposed to insert a phase or update a phase when I
encountered issues during verification. I did both, but neither felt right. Issues that were discovered during
execution by the agent were only noted, but didn't update the phases or plans.

The linear nature also meant I could not parallelize work. I couldn't spin up two agents and have them work on
separate features or problems. The only parallelism that existed was within waves where multiple plans could execute.
In practice, I was sitting around waiting for the agent to finish executing all the plans in a phase. The default scope
of a phase was too large — verification happened too late and the amount of generated code was too much to review well.

In the end, with the poor verification results, I might have actually been less productive than if I had just used
vanilla Claude Code.

# Conclusion

Even though I've been critical, GSD is a full-featured framework and might work well for someone who prefers a linear flow with manual verification.

## My takeaways for coding agent frameworks

In the end, I'm happy I did this experiment. It actually gave me a lot of ideas for what I _do want_ to see in a coding
agent framework. It made me realize that the key problem we are trying to solve with these frameworks is turning our
_tacit_ knowledge of engineering process into _explicit_ knowledge. This is no small deal. It also made me realize
that there are a few other important problems and use cases that need to be accounted for:

- Collaborating with Claude code on feature planning is only the first step. Making it easy to review and iterate on
  the plan is just as important. Integrating review into the workflow at the right points is critical. There has to be
  a balance between how much of these manual review points to introduce in the process to avoid review fatigue.
- Focusing on acceptance criteria in the planning phase should probably be more than 75% of the review effort. More
  time spent in here allows for backpressure and automated testing. This will hopefully yield less manual code review
  and again avoid review fatigue.
- But, at the same time, it should be easy to introduce experiments / spikes and one off improvements in the code
  without the tax of a heavy process or the need to define detailed acceptance criteria (especially if you are
  experimenting with an idea and don't know how it's going to look just yet). Here, you still want as much backpressure
  as possible so that you will get a working prototype to play with instead of going ten rounds with the agent to fix
  stupid issues.
- The design and planning workflow should allow for the agents to work in parallel. If I've done a good job of
  defining and decomposing work and keeping my system modular, I should be able to work on _many_ (dozens, hundreds?) of
  stories and experiments in parallel.
- The product development process should not hard-code everything. The artifacts (issues tracking, documents), the
  review process (PRs in Github or just a conversation with Claude Code), and the tactics of development (how to
  constantly use tools to provide more backpressure signal to the agent, e.g. compiler errors/warnings, linting issues,
  test failures, forcing it to manually exercise the product itself) are all orthogonal to the process itself. I
  realized this when thinking about the lack of back pressure in execution phase. Every project has its own way to
  introduce this backpressure. It doesn't make sense to encode it into the process. As an engineering manager, I can
  conceptually understand a product development process that a PM and I would agree on without having to detail in that
  same process all the steps we will use to ensure that the code is working before we make a PR. The same is true for
  the issue tracking. All of these concerns are inputs into _realizing_ the process in a specific instance.

I will be developing these ideas as they crystallize for me in future posts.

# Appendix - GSD Implementation

Even though I've been rather critical of GSD, I did learn a lot from reading the code for its commands and agents.
There are a lot of _good_ things that GSD implements, like its research command, and how it implements its commands
as idempotent processes. Some of the things that I learned were:

- Breaking out commands into separate sections using XML tags. One of the sections that is common to all commands
  is the `<context>` or `<execution_context>` section which references other files using `@` syntax (which should be
  more token efficient than referring to the file as a string). However, I think with agent skills, you can
  [reference assets using markdown links](https://agentskills.io/specification#file-references).
- Using templates to generate consistent artifacts, such as `PLAN.md`, `SUMMARY.md`, etc.
- I learned that you can induce tool calling by using a function syntax. It was after seeing this that I started
  digging into what tools Claude Code has available to it and learning that I can actually ask Claude Code to tell me
  more about the tools and how to call them. An example of this function syntax are the calls to the
  [`Task` tool](https://code.claude.com/docs/en/settings#tools-available-to-claude) in the
  [`execute-phase` command](https://github.com/galexy/revivo-mk1/blob/main/.claude/commands/gsd/execute-phase.md).

However, this was one part of the code that was harder to understand. The commands would have a higher-level description
of the workflow, but then refer to a workflow file that was more detailed. For example `/gsd:verify-work` would
refer to `@./.claude/get-shit-done/workflows/verify-work.md`. In the command file, the process would be outlined like
this:

```markdown
<process>
1. Check for active UAT sessions (resume or start new)
2. Find SUMMARY.md files for the phase
3. Extract testable deliverables (user-observable outcomes)
4. Create {phase}-UAT.md with test list
5. Present tests one at a time:
   - Show expected behavior
   - Wait for plain text response
   - "yes/y/next" = pass, anything else = issue (severity inferred)
6. Update UAT.md after each response
7. On completion: commit, present summary
8. If issues found:
   - Spawn parallel debug agents to diagnose root causes
   - Spawn gsd-planner in --gaps mode to create fix plans
   - Spawn gsd-plan-checker to verify fix plans
   - Iterate planner ↔ checker until plans pass (max 3)
   - Present ready status with `/clear` then `/gsd:execute-phase`
</process>
```

In the workflow files, there would be a more detailed `<process>` section that had nested named `<step>` sections that
_usually_ corresponded to the steps in the higher-level process. It's not clear to me that the separate file was
necessary and whether the XML syntax for the process is helpful. I think this is worth experimenting with.

One last thing. I was testing GSD on version 1.10.0. Shortly after this release, the author made some improvements and
moved deterministic parts of the process out into scripts, which reportedly reduce token usage.
