import { visit } from 'unist-util-visit';

/**
 * Rehype plugin to convert YouTube URLs in image syntax to embedded videos.
 * Supports Obsidian-style syntax: ![optional title](https://www.youtube.com/watch?v=xyz)
 *
 * Supported URL formats:
 *   - https://www.youtube.com/watch?v=VIDEO_ID
 *   - https://youtube.com/watch?v=VIDEO_ID
 *   - https://youtu.be/VIDEO_ID
 *   - https://www.youtube.com/embed/VIDEO_ID
 */
export default function rehypeYoutubeEmbed() {
  return (tree) => {
    visit(tree, 'element', (node, index, parent) => {
      if (node.tagName !== 'img') return;

      const src = node.properties?.src;
      if (!src) return;

      const videoId = extractYoutubeId(src);
      if (!videoId) return;

      const title = node.properties?.alt || 'YouTube video';

      // Create the iframe element
      const iframe = {
        type: 'element',
        tagName: 'iframe',
        properties: {
          src: `https://www.youtube.com/embed/${videoId}`,
          title: title,
          frameBorder: '0',
          allow: 'accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share',
          allowFullscreen: true,
          loading: 'lazy',
        },
        children: [],
      };

      // Wrap in a responsive container
      const wrapper = {
        type: 'element',
        tagName: 'div',
        properties: {
          className: ['video-embed'],
        },
        children: [iframe],
      };

      // If parent is a figure (from remark-figure-caption), replace the figure content
      if (parent?.tagName === 'figure') {
        // Find and keep figcaption if it exists
        const figcaption = parent.children?.find(
          (child) => child.tagName === 'figcaption'
        );

        // Replace figure children with wrapper and optional figcaption
        parent.children = figcaption ? [wrapper, figcaption] : [wrapper];
      } else if (parent && typeof index === 'number') {
        // Replace image with wrapper
        parent.children.splice(index, 1, wrapper);
      }
    });
  };
}

function extractYoutubeId(url) {
  try {
    const parsed = new URL(url);
    const hostname = parsed.hostname.replace('www.', '');

    if (hostname === 'youtube.com') {
      // Handle /watch?v=ID format
      if (parsed.pathname === '/watch') {
        return parsed.searchParams.get('v');
      }
      // Handle /embed/ID format
      if (parsed.pathname.startsWith('/embed/')) {
        return parsed.pathname.split('/embed/')[1];
      }
    }

    if (hostname === 'youtu.be') {
      // Handle youtu.be/ID format
      return parsed.pathname.slice(1);
    }

    return null;
  } catch {
    return null;
  }
}
