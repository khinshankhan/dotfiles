;; LSP: requires node 16 and pnpm -g add @emacs-grammarly/grammarly-languageserver
(require 'core-straight)
(require 'core-module)

(package! lsp-grammarly
  :if (feature-p! +lsp)
  :config
  (lsp! text-mode
    (auto-ide/add! 'text-mode #'hydra-lsp/body)
    (auto-ide/add! 'markdown-mode #'hydra-lsp/body)))
