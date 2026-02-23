---
title: My second attempt at building the ARCTOS robot
description: A short account of my second and final attempt at building my first 6 DOF robotic arm
createdAt: 2026-02-22T10:23
tags:
  - robotics
draft: true
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

I decided to go down the heat shrink route. So, I disassembled the robot for the second time.
