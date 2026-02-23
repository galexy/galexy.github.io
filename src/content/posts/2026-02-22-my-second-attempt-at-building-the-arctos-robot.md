---
title: My second attempt at building the ARCTOS robot
description: A short account of my second and final attempt at building my first 6 DOF robotic arm
createdAt: 2026-02-22T10:23
tags:
  - robotics
draft: false
---

# Recap

In my last post, I described my first attempt during the summer of 2024 the building the art robot just before I started working on AI agents at Thunk.AI. It ended with me burning out the campus controller, and the motor controller boards for several of the motors. I did order new controller boards, but they were on backorder. So naturally, I stopped working on the robot.

This thing  taunted  I left it in my garage. Every time I would get in my  I’d see this unfinished project. I suspect I always had a bit of ADD growing up. And, the unfinished project was just an uncomfortable reminder of that.

# I should have set better goals in retrospect

So when I left Thunk, the first thing I resolved to do was to take this project to some completion point. I knew it would be frustrating based on my first experience. I knew that due to a combination of the design and use of 3D printed parts for the gears was going to make backlash really hard to get rid of. There was no way that precise control was going to possible. I also knew that the software systems that ARCTOS had been building were just shitty and that learning model robotics using reinforcement learning or VLAs was going to be nearly impossible. So I set the goal of just building out all the axes of the robot and being able to do basic controls with it using ARCTOS’ official software.

I realized that I wasn't interested in the mechanical challenges of robotics, but rather the AI related challenges. In retrospect, I should have decided on a robotics platform with that objective in mind.

But, I didn't want to completely waste the effort. I didn't want to sink too much more time into ARTCOS, so I choose a much more modest goal.

# Figuring out what changed

Several revisions were made since I worked on the project in September of 2024 when I started back up in December of 2025. It looked like there were some good changes that addressed some pain points, but the changelogs were not specific enough. So, the first thing I had to do was go through each printed part and figure out which parts were modified and decided whether they needed to be reprinted. This is another example where good engineering practices were missing. Parts were not labeled with revision numbers, the change log only still general description of changes and not which specific parts were modified and whether they need to be reprinted from revision to revision.

# The Kits were part of the Problems

Backlash in the Y and Z axis gears were a big reason for being frustrated with the build. That's why I had to rip it apart in the first place. Which lead to accidentally re-wiring things incorrectly (mixing polarity to motor control boards) and leading to the motor controller board going up in smoke. So, I wanted to see if I could reduce the backlash.

So, I scoured the discord history since I left and found out that the rods that came in the kits used in the cycloidal gears were not actually manufactured to spec. They were supposed to be 5mm in diameter, were actually 4.5mm or 4.8mm instead. There were claims that this was a huge contributor to the backlash. The only problem was at first, I couldn't order just the new rods anymore.

The maintainer / designer of the robot admitted this was a problem and had resorted to cutting his own rods. One other person used heat shrink tubes around the rods to increase the diameter, but said that the rods were really hard to put in, but seemed to help.

I decided to go down the heat shrink route.

![ARTCOS Robot Take 2](https://youtu.be/q9KYasdJmtI)

So, I disassembled the robot for the second time.

![Die Robot Die](https://youtu.be/zRCi92Za_P8)

# Hacking the rods

After I disassembled the Y and Z axis and had access to all the cycloidal gear rods. I then wrapped the center of each rod with heat tubing and shrink them to fit. The center of each rod was not a bit more than 5mm in diameter. But, it would have been nearly impossible to insert the rods in based on the original order of assembly in the instructions. That's because the gears were originally assembled with the external harness before the rods were inserted. This was impossible for the roads in the Z axis.

But, I was able to come up with alternative order for assembling that allowed me to insert the rods into the gears before the full assembly.

Here I am rebuilding the Y Axis.

![Third time's a charm](https://youtu.be/MDYDLZOp-b0)

And, then rebuilding the Z Axis.

![Z Axis is the Zany Axis](https://youtu.be/p1h0oFCgT9o)

Still more Z Axis rebuilding.

![More rebuilding of Z Axis](https://youtu.be/L82gxhVhR6M)

After all that work, **_most_**, but not all the backlash was gone. Oh well, that's good enough for me. Let's move on.

# It just stopped

I continued the build. I rebuilt the A axis. The A, B and C axis are different the Y and Z axis. These three use planetary gears and were a royal pain in the ass to put together. I wore gloves, but all the screwing wore down the skin on my index finger. When I put the A axis together, it sounded a bit funky, but I can figure what the problem was. I continued and put together the B and C axis structure. _See my first post to read about my rant about these two gears. _

When I put this larger B/C axis structure onto the A axis, it started to wobble. It seemed I hadn't tightened the cover of the A axis enough. So, I did that. And, the A axis stopped worked!!! For some reason, the gears would turn for a few seconds and then just jam up.

I shared this video on discord hoping that someone could help me. No dice.

![A Axis lock up](https://youtube.com/shorts/z1w8_B9t0xk?feature=share)

It took several days of testing multiple hypotheses, disassembling the A axis several times to finally figure out that the gear on the A axis motor was a few millimeters too far out and rubbing up against the A axis planetary gears. The gear actually got all scratched up from this contact. All I had to do was shift the gear back a bit. Why the fuck hadn't the designer anticipated this issue and designed a component to precisely position the gear? Seriously, what the fuck?

![](/images/IMG_6380.jpeg)

It was at this point that I decided, once I reach my goal, I'm done with ARCTOS. What a piece of shit.

The funny thing is that a few days later, a couple of grad students hit the same issue and also pleaded for help on discord. I pointed to my post and my findings. They were skeptical at first. But, a couple of days later they posted that they had disassembled everything down to the A axis and found they had the exact same issue and my suggested fix resolved their problems.

# Just work already

I finished putting all the remaining gears together, but left the clamp off. I just wanted to see if I could actually control the arm from the software. In the time that I was away from working on the robot, the developer of the robot had moved away from open sourcing the software. Instead, he moved to a subscription model. I had already thrown in the towel at that point. But, seriously, what the fuck? 

Thankfully, there a free version that had really stripped down functionality. Basically you could calibrate and move the robot. Fuck it, I'm not paying for anything else.

After a lot of guessing and trial and error, I was able to partially calibrate the robot and got the X, Y, Z and A axis to mostly work. B and C were just impossible due to slippage in the belt.

# Moving on

I'm glad that's all behind me now. I'm moving onto other robotics platforms. Playing with ARCTOS had it ups and downs... mostly downs. I don't think the developer of ARCTOS realizes how frustrating his product is. I paid with real money and time to build this and it was honestly just .... frustrating.

The product lacks cohesive documentation that points out assembly gotchas. But, rather the developer relies on discord discussions to document these issues. People repeatedly answer the same questions over and over again. The developer lacks operational mechanisms for addressing feedback and updating the product and documentation. 

In my career, seen the results of software engineering teams that don't have a learning and continuously improving mindset, that don't design for serviceability, maintainability, ease of deployment and operations (such as lack of observability). What you get is a shitty product experience.

I've learned, from this experience, that software isn't the only type of product that suffers from these poor product and engineering practices. I just always thought that software engineers were lazier and therefore more likely to cut corners because software is so malleable. But, it turns out that poor engineering and design can show up anywhere.
