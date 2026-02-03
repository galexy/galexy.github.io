---
title: "Claude Code's Task tool"
createdAt: "2026-02-03T00:35"
draft: false
tags: ["claude"]
---

Claude Code has a tool, named Task, that didn't make it to the 
[official list of built-in tools](https://platform.claude.com/docs/en/agent-sdk/overview#built-in-tools), 
but is probably one of the most possible tools it has. 

I found it while reading through the [source code](https://github.com/glittercowboy/get-shit-done/blob/2347fca35ead5b4bf93b238afdf30a0f91947391/commands/gsd/plan-phase.md?plain=1#L225)
of the [meta-prompting project Get Shit Done](https://github.com/glittercowboy/get-shit-done). In some of the commands
and "workflows", Claude is instructed to invoke the Task tool, which spawans subagents
to execute a nested prompt. This prevents the all the resulting context generated to execute
that prompt from polluting the current context. I knew that subagents were designed for this,
but previously I thought that the only way that a subagent was spawned was if claude code
decided that the subagent was most appriorate based on its description or via a skill
that had a forked context. 

But, by exposing a tool, a custom slash command or skill can actually invoke that tool.

I couldn't find documentation online for the Task tool, so I asked Claude what the 
arguments to the tool are.

## Task Tool Parameters

| Parameter | Required | Type | Description |
|-----------|----------|------|-------------|
| `prompt` | Yes | string | The task for the agent to perform |
| `subagent_type` | Yes | string | The type of specialized agent to use |
| `description` | Yes | string | Short (3-5 word) description of the task |
| `model` | No | enum | `"sonnet"`, `"opus"`, or `"haiku"` — inherits from parent if not specified |
| `resume` | No | string | Agent ID to resume a previous execution |
| `run_in_background` | No | boolean | Run asynchronously; use `TaskOutput` to check results |
| `max_turns` | No | integer | Max API round-trips before stopping |

## Built-in `subagent_type` Options

| Type | Purpose |
|------|---------|
| `general-purpose` | Complex multi-step tasks with full tool access |
| `Explore` | Fast codebase search/analysis (read-only, uses Haiku) |
| `Plan` | Research for planning mode |
| `Bash` | Command execution specialist |
| `claude-code-guide` | Answer questions about Claude Code itself |

### Example Usage
```
<invoke name="Task">
  <parameter name="prompt">Search for all authentication-related files and explain how login works</parameter>
  <parameter name="subagent_type">Explore</parameter>
  <parameter name="description">Explore auth system</parameter>
</invoke>
With model override:


<invoke name="Task">
  <parameter name="prompt">Implement the user profile feature based on the PLAN.md</parameter>
  <parameter name="subagent_type">general-purpose</parameter>
  <parameter name="description">Execute profile plan</parameter>
  <parameter name="model">opus</parameter>
</invoke>
Background execution:


<invoke name="Task">
  <parameter name="prompt">Run full test suite and report failures</parameter>
  <parameter name="subagent_type">Bash</parameter>
  <parameter name="description">Run tests</parameter>
  <parameter name="run_in_background">true</parameter>
</invoke>
```
