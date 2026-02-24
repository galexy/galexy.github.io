---
title: Experiment with Get Shit Done
description: What I liked and didn't like about the GSD meta-prompting framework
createdAt: 2026-02-23T16:29
tags:
  - Claude Code
  - coding-agent
draft: true
---

I few weeks ago, I realized that "programming" coding agents has become probably more important than being able to use the LLM APIs (Chat Completions, Responses, etc.). This is especially true for those of us that code. But, I believe will be just as true for anyone looking to build autonomous agents as part of their business process and lives. _Aside: It's possible that learning to crawl is just as important too._

There is obviously so much to learn. Where should one start? I'm already very comfortable pair programming with Claude Code, working with CLAUDE.md files and event creating simple custom slash commands. But, there are subagents, teams, tasks, skills just in CC proper. And, there are the tools like beads and toolkits like SuperClaude, and orchestration frameworks like claude-flow and Get Shit Done.

I could approach it from the bottoms up. But, I decided to try some of the more _complete_ frameworks and to see what the experience developing with them was like. I also wanted to learn from their code to learn how Claude Code features were being used in the wild. My guess was that I would end up back at the bottom after going through this exercise, because I suspected learning to use these frameworks was akin to learning jQuery in the early days of Web 2.0. Useful, because you get some high-level reps. But, ultimately it still too early of a framework to define how we would all eventually work.

# Defining the experiment

I wanted a way evaluate these each framework consistently. So, I decided to build the same application from a green field repo. I choose to build a very stripped down version of a Quicken clone that would have a python backend and a react frontend. I didn't need to completely build the product. But, I wanted the framework to have the context that I was building a medium size production application. My goal was to form a **_subjective_** opinion on how effective the framework was at helping me build develop the product.

## Some Constraints

- Develop the solution without having to manually write any of the code
- Only minimally review the generated code
- Provide only requirements, constraints, and architectural guidance

## My Evaluation Criteria

My evaluation was roughly consistent of:

- Did I feel like the coding agent was generally building good solution given reasonable requirements, constraints and test specifications?
- Did I feel like the coding agent was generally doing a good job of fixing issues on its own as it was developing and would only consider its work complete after getting a solution that worked on the happy path? I didn't expect it to do a good job of finding edge cases?
- Was the workflow smooth and did it make iteration easy? Iteration includes both working through features from some backlog, but also maintaining and updating that backlog as the product progressed and ideas and/or issues arose.

Separately, I would spend some time looking at the framework's code in the hopes that I could learn patterns for how to program Claude Code myself and whether I thought prompts and workflows were effective. This is very **_subjective_**. But, a year spent building a no-code and natural language only AI Agent workflow system for enterprises and actually building solutions for real customers taught me that there are real patterns for "programming" natural language agents. And, that there is _good _and _bad _ways to do it.

# My first experiment
