(require 'core-straight)
(require 'core-module)

(with-feature! +mdx
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode)))
