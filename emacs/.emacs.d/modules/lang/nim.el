;; LSP: nimble install nimlangserver
(require 'core-straight)

(package! nim-mode
  :mode ("\\.nim\\'" . nim-mode))

(lsp! nim-mode
  (auto-ide/add! 'nim-mode #'hydra-lsp/body))
