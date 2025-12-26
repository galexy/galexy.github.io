// Enhance code blocks with language labels and copy buttons
document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll("pre.astro-code").forEach((pre) => {
    // Create wrapper container
    const wrapper = document.createElement("div");
    wrapper.className = "highlight";
    pre.parentNode?.insertBefore(wrapper, pre);
    wrapper.appendChild(pre);

    // Get language from data attribute
    const language = pre.getAttribute("data-language") || "text";

    // Create title bar with language label
    const titleBar = document.createElement("div");
    titleBar.className = "code-title";
    titleBar.textContent = language;

    // Add copy button if clipboard API is available
    if (navigator.clipboard) {
      const copyButton = document.createElement("button");
      copyButton.className = "copy-button";
      copyButton.textContent = "Copy";

      copyButton.addEventListener("click", async () => {
        const code = pre.textContent || "";
        try {
          await navigator.clipboard.writeText(code);
          copyButton.textContent = "Copied";
          setTimeout(() => {
            copyButton.textContent = "Copy";
          }, 1000);
        } catch (error) {
          console.error("Failed to copy:", error);
        }
      });

      titleBar.appendChild(copyButton);
    }

    // Insert title bar before the code
    wrapper.insertBefore(titleBar, pre);
  });
});
