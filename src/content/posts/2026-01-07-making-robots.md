---
title: "Starting my robot making journey"
createdAt: "2026-01-07T16:48"
tags: ["learning", "3D Printing", "robots"]
draft: true
---

I left Thunk AI at the end of November of last year. I spent just over a bit over a
year there as an engineer working on the the AI agent platform. I'm proud of the work.
But, I decided to pull back from Thunk for the moment. I've gone back into learning
mode like the six or so months before I joined Thunk. There is something amazing when
you are afforded the time to look around and really learn new things. This is difficult
when you're heads down working on making a product like I have for most of my career.
I don't know how true innovators find the mental bandwidth to bring new things into
their existing work. That is truly a skill.

So now I have the time again to learn new things. I decided to get back to a project
that I paused when I joined Thunk, the Arctos Robot. I started this building my robot
arm in just before summer of 2024. At the time, I didn't have a specific goal in mind. 
I wanted to learn how to apply RL and robotics seems like the right place. I didn't
do a great deal of research before deciding to build the Arctos Robot. I was initially
really impressed with how it looked and thought it would be cool to learn about the
mechanics of a robot rather than purchase an assembled arm. The only "due diligence"
I did was watch a little bit of a recorded livestream linked from Arctos' website by
an engineer going by the moniker "Unexpected Maker". Each stream was several hours long
and there were multiple videos. I fast forwarded through several videos and thought, 
"Well, he's was able to build it and I can follow along." I tell you all this, because
in retrospect, I should have done more homework before choosing to build this arm.

I paid the ~$45 to get the CAD and STL files and started printing on my recently acquired
Prusa MK4. I should note, my experience 3D printing up this point was mostly watching
my son print toys on a friend's Prusa mini. I had *never* soldered before. My exposure
to electronics was doing a few experiments from the Make: Electronics book and putting
together a waveshare RC car over Thanksgiving break in like 2017. I didn't know how to
use any CAD software like Fusion 360. I did not know what I was getting myself into. 
I watched a bit more of the Unexpceted Maker's videos trying to pick up tips as he was
building. And, I virtually flipped through the only real documentation which was the 
assembly guide while the first parts for the X-axis was printing. I lurked on the 
discord channel, which was difficult to navigate. Getting the X-axis was pretty 
straightforward and I thought to myself, "this isn't going to be so bad."

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
After a lot of head scratching and I figured out how to configure and wire the 
motors up outside of the arm.

I slowly printed the parts (some taking 20+ hours) and put together the arm up to the A-axis
over a month and a half. 
