---
title: "ReLU^2"
createdAt: 2025-12-27T22:14 
tags: ["deep learning"]
draft: false
---

Google published [Primer: Searching for Efficient Transformers for Language Modeling](https://arxiv.org/abs/2109.08668)
in 2021 that describes the an variant of the ReLU activation
function that is simply the square of ReLU. It was discovered
using an automated/evolution based technique for searching
for an improved Transformer model built using simple 
TensorFlow functions as primitives.

I found this insight in the paper particularly interesting.

> Our objective is different in that the
trade-off between step time and sample efficiency is implicit. For instance, a modification that doubles
step time, but triples sample efficiency is a good modification in our search, as it ultimately makes the
architecture more compute efficient. Indeed, the modifications we find to be most beneficial, squaring
ReLUs and adding depthwise convolutions to attention, increase training step time. However, they
improve the sample efficiency of the model so much that they decrease the total compute needed
to reach a target quality, by drastically reducing the number of training steps needed to get there.