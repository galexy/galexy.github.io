---
title: Forwarding xdg-open
url: https://blog.rymcg.tech/blog/linux/ssh-remote-xdg-open/
createdAt: 2026-03-10T22:48
tags:
  - linux
draft: false
---

I noticed that fly.io's sprite video demonstrated that they somehow forwarded xdg-open calls and your local browser would open when Claude Code auth was requested even though it was running on a remote firecracker vm. I found this [post](https://blog.rymcg.tech/blog/linux/ssh-remote-xdg-open/) that describes how to implement this. I ending porting this to my [devcontainer setup](https://github.com/galexy/claude-code-starter/commit/95106d1b337350ac93534e5274d49d6ae720735d#diff-7770cd17ba9a571dac1da349722b696b5f988722cb4db147ca2da0752a320aa8) and even got it to work even when I ssh into a linux box running devcontainers _from my Mac._
