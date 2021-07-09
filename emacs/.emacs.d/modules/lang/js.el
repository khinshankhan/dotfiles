(require 'core-straight)

;; tools
(package! add-node-modules-path
  :hook
  ((html-mode
    css-mode
    web-mode
    markdown-mode
    js-mode
    js2-mode
    json-mode
    rjsx-mode
    typescript-mode
    typescript-tsx-mode
    solidity-mode) . add-node-modules-path))

(package! prettier-js
  :hook
  ((html-mode
    css-mode
    web-mode
    markdown-mode
    js-mode
    js2-mode
    json-mode
    rjsx-mode
    typescript-mode
    typescript-tsx-mode
    solidity-mode) . prettier-js-mode))

(after! yasnippet
  (package! react-snippets))
