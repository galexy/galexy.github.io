---
title: "Learning Rust in 2025"
description: "My journey learning the Rust programming language"
createdAt: 2025-12-25
tags: ["rust", "programming", "learning"]
draft: false
---

I've started learning Rust, and it's been quite an adventure! Here are my initial thoughts.

## Why Rust?

Rust offers several compelling features:

- Memory safety without garbage collection
- Zero-cost abstractions
- Fearless concurrency
- Great tooling (Cargo, rustfmt, clippy)

## First Impressions

The ownership system takes some getting used to, but it forces you to think about memory management explicitly.

Here's a simple example:

```rust
fn main() {
    let mut counter = 0;

    for i in 1..=10 {
        counter += i;
    }

    println!("Sum: {}", counter);
}
```

## The Learning Curve

The Rust compiler is strict, but its error messages are incredibly helpful. It's like having a mentor guiding you.

## Resources

Some great resources I've found:

- The Rust Book (official documentation)
- Rust by Example
- Rustlings exercises
- The Rust community on Discord

Looking forward to building more with Rust!
