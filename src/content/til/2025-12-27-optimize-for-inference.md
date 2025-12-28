---
title: "optimize_for_inference fuses batch normalization"
createdAt: 2025-12-27T21:14
tags: ["pytorch"]
draft: false
---

[`optimize_for_inference`](https://docs.pytorch.org/docs/stable/generated/torch.jit.optimize_for_inference.html)
isn't a function that is usually presented in beginner 
materials for pytorch. I just read about it in [Aurélien 
Géron's](https://x.com/aureliengeron) new edition of his
hands on ML book. This edition (considered a 1st edition) 
focuses on pytorch in Part II. I loved all his previous
editions because he really takes beginners on a deep-dive on 
DL and generously sprinkles links to recent/important papers 
on each of the topics. In this case, he explains that the 
function `optomize_for_inference` performs some fusion of 
batch normalization with previous linear layers.

Batch normalize will learn $\gamma$ and $\beta$ which will
scale and shift the inputs to the BN layer after normalizing
them (using $\mu$ and $\sigma$). So, if the previous layer
computes $XW + b$, then the BN layer will compute $\gamma 
\oplus (XW + b - \mu) / \sigma + \beta$. The BN layer can
be eliminated if we let $W' = \gamma \oplus W / \sigma$
and $b' = \gamma \oplus (b - \mu) / \sigma + \beta$.

It appears that fusion with CNN layers has been a well
established technique for [runtime/inference](https://leimao.github.io/blog/Neural-Network-Batch-Normalization-Fusion/)
and it's even possible to [reduce the memory requirements
during training](https://docs.pytorch.org/tutorials/intermediate/custom_function_conv_bn_tutorial.html) 
by recognizing that fusion will occur.