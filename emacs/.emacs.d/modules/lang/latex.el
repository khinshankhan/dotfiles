(require 'core-straight)

(package! auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

(lsp! LaTeX-mode
  (auto-ide/add! 'LaTeX-mode #'hydra-lsp/body))
