(require 'core-straight)
(require 'core-module)

(use-package lsp-grammarly
  :if (feature-p! +lsp)
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp)))
  :config
  (shan--ide-add 'text-mode #'hydra-lsp/body)
  (shan--ide-add 'markdown-mode #'hydra-lsp/body))
