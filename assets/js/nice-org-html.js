/* Personal behavior for nice-org-html exports. */

const personalTitle = document.querySelector("#content > h1.title");

if (personalTitle) {
    const titleTextWalker = document.createTreeWalker(
        personalTitle,
        NodeFilter.SHOW_TEXT
    );
    let titleTextNode = titleTextWalker.nextNode();

    while (titleTextNode && !/\S/.test(titleTextNode.nodeValue)) {
        titleTextNode = titleTextWalker.nextNode();
    }

    if (titleTextNode) {
        const firstWordMatch = titleTextNode.nodeValue.match(/^(\s*)(\S+)/);

        if (firstWordMatch) {
            const firstWord = document.createElement("span");
            firstWord.className = "title-first-word";
            firstWord.textContent = firstWordMatch[2];

            const remainder = titleTextNode.splitText(firstWordMatch[0].length);
            titleTextNode.nodeValue = firstWordMatch[1];
            remainder.parentNode.insertBefore(firstWord, remainder);
        }
    }
}
