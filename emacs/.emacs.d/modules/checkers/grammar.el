(require 'core-straight)
(require 'core-module)

(package! lsp-grammarly
  :if (feature-p! +lsp)
  :ensure t
  :config
  (lsp! text-mode
    (auto-ide/add! 'text-mode #'hydra-lsp/body)
    (auto-ide/add! 'markdown-mode #'hydra-lsp/body)))
