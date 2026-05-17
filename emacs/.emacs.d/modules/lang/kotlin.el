;; LSP: github.com/fwcd/kotlin-language-server
(require 'core-straight)

(package! kotlin-mode
  :mode ("\\.kt\\'" . kotlin-mode))

(lsp! kotlin-mode
  (auto-ide/add! 'kotlin-mode #'hydra-lsp/body))
