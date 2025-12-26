---
layout: ../layouts/BaseLayout.astro
title: Style Showcase
description: Showcase of all terminal theme styling elements
---

# Terminal Theme Showcase

This page demonstrates all the styling elements available in the terminal theme. From typography to forms, everything has been ported from the original theme.

## Headings

# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6

---

## Text Formatting

This is a paragraph with **bold text**, *italic text*, and `inline code`. You can also have [links](#) within text.

---

## Blockquotes

> This is a blockquote. It is used for quoting other sources or highlighting important information. The terminal theme adds a special ">" character at the beginning.
>
> Blockquotes can contain multiple paragraphs.

---

## Lists

### Unordered List

- First item with a dash prefix
- Second item
- Third item with nested list:
  - Nested item 1
  - Nested item 2
- Fourth item

### Ordered List

1. First numbered item
2. Second numbered item
3. Third numbered item

---

## Code Blocks

Inline code looks like this: `const x = 42;`

Code blocks have syntax highlighting and a language label with copy button:

```javascript
function greet(name) {
  console.log(`Hello, ${name}!`);
  return true;
}

// Call the function
greet('World');
```

Here's a Python example:

```python
def calculate_fibonacci(n):
    if n <= 1:
        return n
    return calculate_fibonacci(n-1) + calculate_fibonacci(n-2)

# Calculate the 10th Fibonacci number
result = calculate_fibonacci(10)
print(f"Fibonacci(10) = {result}")
```

And some Rust:

```rust
fn main() {
    let numbers = vec![1, 2, 3, 4, 5];

    let sum: i32 = numbers.iter().sum();
    println!("Sum: {}", sum);

    let doubled: Vec<i32> = numbers.iter()
        .map(|&x| x * 2)
        .collect();
    println!("Doubled: {:?}", doubled);
}
```

---

## Tables

| Name    | Age | Occupation |
|---------|-----|------------|
| Alice   | 30  | Developer  |
| Bob     | 25  | Designer   |
| Charlie | 35  | Manager    |

---

## Forms

<form>
  <div style="margin-bottom: 15px;">
    <label for="name" style="display: block; margin-bottom: 5px;">Name:</label>
    <input type="text" id="name" name="name" placeholder="Enter your name" />
  </div>

  <div style="margin-bottom: 15px;">
    <label for="email" style="display: block; margin-bottom: 5px;">Email:</label>
    <input type="email" id="email" name="email" placeholder="your@email.com" />
  </div>

  <div style="margin-bottom: 15px;">
    <label for="message" style="display: block; margin-bottom: 5px;">Message:</label>
    <textarea id="message" name="message" rows="4" placeholder="Your message here..."></textarea>
  </div>

  <div style="margin-bottom: 15px;">
    <label for="country" style="display: block; margin-bottom: 5px;">Country:</label>
    <select id="country" name="country">
      <option value="">Select a country</option>
      <option value="us">United States</option>
      <option value="uk">United Kingdom</option>
      <option value="ca">Canada</option>
    </select>
  </div>

  <div style="margin-bottom: 15px;">
    <label>
      <input type="checkbox" name="subscribe" />
      Subscribe to newsletter
    </label>
  </div>

  <div style="margin-bottom: 15px;">
    <label style="display: block; margin-bottom: 5px;">Preferred contact:</label>
    <label style="display: block; margin-bottom: 5px;">
      <input type="radio" name="contact" value="email" />
      Email
    </label>
    <label style="display: block;">
      <input type="radio" name="contact" value="phone" />
      Phone
    </label>
  </div>

  <button type="submit">Submit Form</button>
</form>

---

## Buttons

<div style="display: flex; gap: 10px; flex-wrap: wrap; margin: 20px 0;">
  <button>Primary Button</button>
  <a href="#" class="button">Link Button</a>
</div>

---

## Text Selection

Try selecting this text! The selection color uses the accent color with the background color for contrast, creating a true terminal feel.
