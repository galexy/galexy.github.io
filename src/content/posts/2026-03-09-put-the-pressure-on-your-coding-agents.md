---
title: Put the pressure on your coding agents
description: More about the need for back pressure in coding agents
createdAt: 2026-03-08T21:36
tags:
  - coding-agents
draft: false
---

I just posted my [review](/2026/03/08/experiment-with-get-shit-done/) of GSD and one of the key points I made was that the framework doesn't induce back pressure on the coding agents. This resulted in reward hacking and in the early phases al the verification stages failing immediately. 

I stole the term "back pressure for coding agents" from this [post](https://banay.me/dont-waste-your-backpressure/). Last July or August, I was said to Tony Fader that I thought that coding agents were the first to show a lot of promise in the agent space because of the vast training data that has been open sourced. He said, it's because code can be verified. That was the first aha moment for me. (Darn it Tony, why are you so smart!?!?)

But, when I read this post in January, I realized that the principle is far more general. And, that it didn't apply to coding agents only. It applied to us human software engineers too. One of the funny things that this back pressure has resulted in is developers who hated things like TDD or static typing are all of a sudden [fans of TDD](https://simonwillison.net/guides/agentic-engineering-patterns/red-green-tdd/) and [moving to statically typed languages](https://simonwillison.net/2026/Feb/18/typing/)! 

I had been using statically typed languages since 1993. My first programming language was QBASIC, but 6 months later, I was learning Pascal for my AP Comp Sci A class and my first real IDE was Turbo Pascal 5. But, the first time I actually internalized back pressure was working with Colin Meek on StreamInsight 1.2. You see, Colin had just finished a stint working for Erik Meyer on LINQ and had become a functional programming master. Colin convinced Sethu, our PUM, to take me under his wing and teach me the ways. We started pair programming (actually we didn't, because Colin ended up doing all the typing). And, what I immediately saw rocked my world. Colin started modeling every problem as a series of types. And, then he would string together the skeleton of the code merely through types and function calls that did nothing but use those types. Then, he would compile the code and rapidly repeat this process until all the types checked. The code did nothing - repeat nothing. It was just types and empty functions using those types. Then he would add a little bit of code and repeat this compiler process over and over again.

At first this seems pointless. But, very soon, he as he put real code (meat on the skeleton) he realized that the problem required that he make some serious changes. He refactored the code and immediately ran the compiler. It would yell at him and he knew exactly where he had to change the code. He did this so rapidly, I couldn't keep up. This was the beginning of my long journey learning functional programming.

Years later when I saw PHP programmers lose their minds while making a change, I would chuckle to myself as I coded in Scala.

Back to the present, I think we're all beginning to understand how automating the feedback signal gives the agents a reward to seek and the result is that the code you review is generally better. This is no different than a human writing an essay. When I was leading the business analytics team at Remitly, I had to push back on junior analysts whenever they sent me whatever analysis they worked on. Guess what your first draft sucks. (By the way, my GSD review post took four drafts to write.)

Everyone is talking about [review fatigue](https://siddhantkhare.com/writing/ai-fatigue-is-real). Yes, it's real. But, we should stop banging our heads. We should be thinking about how to introduce the back pressure. The low-hanging fruit includes, compiler errors, listing errors, [adversarial code review agents](https://www.reddit.com/r/ClaudeCode/comments/1rnfn9p/protip_adversarial_reviews_are_stupidly_easy_and/), pushing agents to clean up after themselves. But, I think the key area is going to be testing.

We should be _reviewing_ the test plan before the code is written. We should have adversarial testing agent create tests that aren't visible to the coding agents. We should be asking the agent to prove itself. This last one is one of my favorite tricks that I picked up after watching an interview with Steve Yegge. Simon Willison even says that our jobs are now [proving that the code we deliver actually works](https://simonwillison.net/2025/Dec/18/code-proven-to-work/). 

We, as an industry, need to start thinking creatively about how to do this. I feel like I've come full circle. My first role at Microsoft in 2004, was Software Development Engineer in Test aka SDET. I transitioned to SDE in 2007, but I consider my time as an SDET to be more valuable. It requires a different way of thinking. Honestly, I practically failed my skip-level's interview when he asked me how would I test \`xcopy\`. It didn't occur to me how to structure my thinking when it came to testing. The funny thing is that Microsoft killed this role in 2011 or 2012, just after I left. We might not see this title anymore, but I bet for the immediate future, a lot of people are going to essentially be SDETs.
