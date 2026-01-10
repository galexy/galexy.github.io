import { visit } from 'unist-util-visit';

const ALIGNMENTS = ['left', 'center', 'right'];

/**
 * Rehype plugin to support Obsidian-style image sizing and alignment.
 * Syntax: ![Alt|WIDTHxHEIGHT|ALIGN](url) or ![Alt|WIDTH|ALIGN](url)
 *
 * Examples:
 *   ![Photo|100x145](image.jpg) -> width="100" height="145"
 *   ![Photo|200](image.jpg) -> width="200"
 *   ![Photo|200|center](image.jpg) -> width="200" + centered
 *   ![Photo|center](image.jpg) -> centered only
 */
export default function rehypeObsidianImages() {
  return (tree) => {
    visit(tree, 'element', (node, _index, parent) => {
      if (node.tagName !== 'img') return;

      const alt = node.properties?.alt;
      if (!alt || !alt.includes('|')) return;

      const { altText, width, height, alignment } = parseAlt(alt);

      // Update image properties
      node.properties.alt = altText;
      if (width) node.properties.width = width;
      if (height) node.properties.height = height;

      // Apply alignment class to parent figure if it exists, otherwise to the image
      if (alignment) {
        const alignClass = `img-${alignment}`;
        if (parent?.tagName === 'figure') {
          addClass(parent, alignClass);
        } else {
          addClass(node, alignClass);
        }
      }

      // Also clean up figcaption if it exists (remark-figure-caption captures raw alt)
      if (parent?.tagName === 'figure') {
        const figcaption = parent.children?.find(
          (child) => child.tagName === 'figcaption'
        );
        if (figcaption) {
          cleanFigcaption(figcaption);
        }
      }
    });
  };
}

function addClass(node, className) {
  if (!node.properties) node.properties = {};
  const existing = node.properties.className;
  if (!existing) {
    node.properties.className = [className];
  } else if (Array.isArray(existing)) {
    existing.push(className);
  } else {
    node.properties.className = [existing, className];
  }
}

function cleanFigcaption(figcaption) {
  // Recursively find and clean text nodes that contain the | syntax
  visit(figcaption, 'text', (textNode) => {
    if (textNode.value && textNode.value.includes('|')) {
      const { altText } = parseAlt(textNode.value);
      textNode.value = altText;
    }
  });
}

function parseAlt(alt) {
  const parts = alt.split('|');
  const altText = parts[0].trim();
  let width = null;
  let height = null;
  let alignment = null;

  for (let i = 1; i < parts.length; i++) {
    const part = parts[i].trim().toLowerCase();

    if (ALIGNMENTS.includes(part)) {
      alignment = part;
    } else {
      // Try to parse as dimensions (WIDTHxHEIGHT or WIDTH)
      const dimMatch = part.match(/^(\d+)(?:x(\d+))?$/);
      if (dimMatch) {
        width = dimMatch[1];
        if (dimMatch[2]) height = dimMatch[2];
      }
    }
  }

  return { altText, width, height, alignment };
}
