(require 'core-straight)
(require 'core-module)

(package! yasnippet
  :config
  (yas-global-mode 1))

(package! yasnippet-snippets
  :if (feature-p! +snippets))

(when (< (length yas-snippet-dirs) 2)
  (yas-reload-all))
