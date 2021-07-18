(require 'core-straight)
(require 'core-module)

(package! yasnippet
  :config
  ;; disable annoying messages
  ;; TODO: trigger to 4 when refining...
  (setq yas-verbosity 0)
  (yas-global-mode 1))

(package! yasnippet-snippets
  :if (feature-p! +snippets))

(when (< (length yas-snippet-dirs) 2)
  (yas-reload-all))
