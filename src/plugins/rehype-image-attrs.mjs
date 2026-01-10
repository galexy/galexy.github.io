import { visit } from 'unist-util-visit';

/**
 * Rehype plugin to add attributes to images using {key=value} syntax.
 * Works by finding text nodes containing {attrs} after images and applying them.
 */
export default function rehypeImageAttrs() {
  return (tree) => {
    visit(tree, 'element', (node, index, parent) => {
      if (node.tagName !== 'img') return;
      if (!parent || !parent.children) return;

      const imgIndex = parent.children.indexOf(node);
      const next = parent.children[imgIndex + 1];

      // Check if next sibling is a text node starting with {
      if (next && next.type === 'text' && next.value.trim().startsWith('{')) {
        const match = next.value.match(/^\s*\{([^}]+)\}(.*)$/);
        if (match) {
          const attrs = parseAttrs(match[1]);

          // Apply attributes to image
          node.properties = node.properties || {};
          Object.assign(node.properties, attrs);

          // Remove or update the text node
          if (match[2].trim()) {
            next.value = match[2];
          } else {
            parent.children.splice(imgIndex + 1, 1);
          }
        }
      }
    });
  };
}

function parseAttrs(attrString) {
  const attrs = {};
  const parts = attrString.trim().split(/\s+/);

  for (const part of parts) {
    if (part.startsWith('.')) {
      // Class
      const existing = attrs.className || '';
      attrs.className = existing ? `${existing} ${part.slice(1)}` : part.slice(1);
    } else if (part.startsWith('#')) {
      // ID
      attrs.id = part.slice(1);
    } else if (part.includes('=')) {
      // Key=value pair
      const eqIndex = part.indexOf('=');
      const key = part.slice(0, eqIndex);
      const value = part.slice(eqIndex + 1).replace(/^["']|["']$/g, '');
      attrs[key] = value;
    }
  }

  return attrs;
}
