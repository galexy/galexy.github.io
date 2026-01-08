---
title: "My start in building robots"
createdAt: "2026-01-07T16:48"
tags: ["learning", "3D Printing", "robots"]
draft: true
---

## Preamble

I left Thunk AI at the end of November of last year. I spent just over a bit over a
year there as an engineer working on the the AI agent platform. I'm proud of the work.
But, I decided to pull back from Thunk for the moment. I've gone back into learning
mode like the six or so months before I joined Thunk. There is something amazing when
you are afforded the time to look around and really learn new things. This is difficult
when you're heads down working on making a product like I have for most of my career.
I don't know how true innovators find the mental bandwidth to bring new things into
their existing work. That is truly a skill.

So now I have the time again to learn new things. Lately, I've been getting into
robotics. I want to share what my experience has been like getting into this field
coming from over 30 years of doing nothing but software.

This post is part one of a series documenting this journey. In this first part, I share 
with you my initial robot arm build that I started before I joined Thunk back in 2024.

## I wanted to just build and learn something

I started this building my robot arm in just before summer of 2024. At the time, I 
didn't have a specific goal in mind. I wanted to learn how to apply RL and robotics 
seems like the right place. I didn't do a great deal of research before deciding to 
build the Arctos Robot. I was initially really impressed with how it looked and thought 
it would be cool to learn about the mechanics of a robot rather than purchase an assembled 
arm. The only "due diligence" I did was watch a little bit of a recorded livestream 
linked from Arctos' website by an engineer going by the moniker "Unexpected Maker". 
Each stream was several hours long and there were multiple videos. I fast forwarded 
through several videos and thought, "Well, he's was able to build it and I can follow along." 
I tell you all this, because in retrospect, I should have done more homework before 
choosing to build this arm.

## Take my money

I paid the ~$45 to get the CAD and STL files (which was a janky experience) and 
started printing on my recently acquired Prusa MK4. I should note, my experience 
3D printing up this point was mostly watching my son print toys (Dummy 13s) on 
a friend's Prusa mini. I had *never* soldered before. My exposure to electronics 
was doing a few experiments from the Make: Electronics book and putting together a 
waveshare RC car over Thanksgiving break in like 2017. I didn't know how to
use any CAD software like Fusion 360. I did not know what I was getting myself into. 
I watched a bit more of the Unexpceted Maker's videos trying to pick up tips as he was
building. And, I virtually flipped through the only real documentation which was the 
assembly guide while the first parts for the X-axis was printing. I lurked on the 
discord channel, which was difficult to navigate. Getting the X-axis was pretty 
straightforward and I thought to myself, "this isn't going to be so bad."

## How do you RTFM when there is no M?

I proceeded to put the Y axis together and then it became clear, I need to tear it apart
and start doing the wiring because it appeared that there was no room for working
with the wires after assembly. But, the assembly manual didn't talk about wiring at 
all and the slim documentation and videos by Arctos himself on the website only 
discussed how to configure the stepper motors and there was nothing about how the wiring
should actually flow through the arm. To make things worse, there are two ways for
wiring the robot and controlling the motors, an "open loop" and a "closed loop" method.
I had purchased the closed loop because that's what Unexpected Maker did. The motors
had a controller attached to them that communicated with another controller board
of a CAN bus. The example scripts for running the motors took G-Code as input and
converted to the CAN messages. But, it all seemed like magic, because there was
virtually no documentation.

It was a this point that it was clear to me that the documentation was incomplete and
often out of date. There was no clear issues list to be found and the only real support
was from the kindness of others on the discord server. I had to browse through the 
history of multiple channels and review Unexpected Maker's videos to pick up wiring tips.
Even he was struggling with putting the arm together and in one video admitted that he
had to tear his arm apart because he also didn't run the wiring as he was assembling.
I found messages where people shared links to Makerbase's documentation on the CAN
message formats. I realized that I needed to plan the build out more in advanced.
I spent time studying the design in Fusion 360. And, at the same time planned out
the wiring. After a lot of trial and error, I figured out how to configure and wire the 
motors up outside of the arm.

## Push through

I slowly printed the parts (some taking 20+ hours) and put together the arm up to the 
A-axis over a few weks. I was feeling proud of myself because I thought in spite
of the lack of documentation, I was able to put it together. That was two cyclodial gears
for the Y and Z axis. I had never worked on anything mechanical before and it was fun
to learn. But, then I started working on the plantetary gears for the A/B/C axis. The
assembly was really painful. I don't know what the decisions went into the design, but
I believe this to be not designed to be assembled easily. The hardest part was keeping
the parts of the gears in place while screwing them together within the semi-assembled
gear system. I was inspired by some of the "tool" parts that came with the Prusa MK4 and 
MMU3 kits that were meant only to assist in the assembly. So, I ended designing and 3D 
printing tool that merely held the gear parts in place.

When I did some initial assembly tests of the B and C axis, it became clear that something
was wrong. The arm made sudden jumps when the Y or Z axis were moving with the A, B and C
axis attached. With the extra weight, it became clear that I had a lot of backlash in the
Y and Z axis. I noticed some play while assembling, but didn't think anything of it. Again,
I had not mechanical engineering experience and didn't even know what backlash was, let alone
have the initution that the backlash I was seeing while assembling was *no bueno*.

## How does it work?

I proceeded to debug the problem. Tearing things almost all the way back down and putting
it back together a couple of times. I tried to isolate why I was getting so much play
in the Y and Z axis. I posted pictures on the discord channel. No help. I parallel, I
was trying to understand how the B and C axis worked. There are two motors, but they
do not independently control a single axis like the rest of the motors. I was confused as
hell how this was going to work. I searched the discord and it turned out that Unexpected
Maker had the very same questions. He asked around and the responses that proceeded snowballed
into a real mess. I think this interaction eventually caused Unexpected Maker to leave
the channel and abandon the project.

## I don't ring the bell, I blow it up

At this point, I was like, it doesn't need to be perfect - let's move forward and get something working
end to end. But, then as I was reassembling the A-axis, I accidently swicth the polarity
of the power cables and I smelled "magic smoke" come out of one of the controller boards.
The makerbase boards didn't have any protection for this foot gun. And, it appeared that
I had successfully destroyed two motor controller boards and even the CAN bus controller.

I had to reorder the parts and it just so happened to be backed ordered. And, I was just
about to start a new job at Thunk.ai. So, I put the arm on hold and it sat in my garage
workbench for over a year just collecting dust.

# Next

In the next post, I pick up where I left off with the Arctos Robot after stepping back from Thunk.
