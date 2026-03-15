---
title: Sandboxing Agents
createdAt: 2026-03-14T17:12
tags:
  - coding-agents
  - security
  - sandbox
  - prompt-injection
draft: false
---

There are so many security concerns that we'll have to face wrt to agents in 2026 and beyond. Exfiltration of [data](https://www.promptarmor.com/resources/claude-cowork-exfiltrates-files) and [security keys](https://www.reddit.com/r/ClaudeAI/comments/1r186gl/my_agent_stole_my_api_keys/) is already a real thing. Sandboxing agents is going to become a real concern in 2026. At the moment, it really gets in the way of DX.

Anthropic has lightweight process sandboxing. They open sourced the SRT library which uses Mac OS kernel's sandbox (aka seatbelt) support and bubblewrap on Linux. These basically ask the kernel to constraint the process. In the case of Mac OS, sandbox takes a profile for the constraints that looks like a scheme DSL. This is the basis for the Application Sandbox. Apple has actually depreciate the lower level API. But, Anthropic decided to use it anyway. 

You can use this sandboxing in Claude Code via the [\`/sandbox\` command](https://code.claude.com/docs/en/sandboxing#enable-sandboxing). The problem with this sandbox is that only the \`bash\` tool call is constrained. The \`Read\` tool needs to be separately constrained via file permissions. Network isolation is handled with a proxy. Claude Cowork goes a bit further with a [lightweight vm](https://gist.github.com/simonw/35732f187edbe4fbd0bf976d013f22c8).

Running hypervisors are the next step. That's what Claude Cowork and Docker Sandboxes are doing. Nanoclaw can run on docker sandboxes natively now for isolating agents. A full VM is required, because Docker containers aren't safe if you need to run Docker containers within them (Docker in docker). The issue is that the opening the docker socket that allows for this use case is fundamentally unsafe. [It allows root access to the host machine](https://cheatsheetseries.owasp.org/cheatsheets/Docker_Security_Cheat_Sheet.html#:~:text=a%20vulnerable%20host.-,RULE%20%231%20%2D%20Do%20not%20expose%20the%20Docker%20daemon%20socket%20(,this%20following%20Docker%20official%20documentation.). So that's a strike against using[ Claude Code in devcontainers](https://code.claude.com/docs/en/devcontainer).

In the case of Docker Sandbox, you still have the issue of secrets being accessible. And, file permissions denying access to files like \`.env\` are not enough. This person found that their agent was able to [sneak around this limitation by using docker itself](https://www.reddit.com/r/ClaudeAI/comments/1r186gl/my_agent_stole_my_api_keys/). In this case, the agent was able to use [\`docker compose --env\` and interpolation](https://docs.docker.com/compose/how-tos/environment-variables/variable-interpolation/#ways-to-set-variables-with-interpolation) to read the secrets. \*Note: that this defeats both sandbox and file permissions in the non-VM case.\* Docker Sandbox uses a [proxy that injects credentials for known agent APIs](https://docs.docker.com/ai/sandboxes/architecture/#credential-injection). But, that doesn't help me for the 36 other API keys I have.

We clearly have a long way to go.
