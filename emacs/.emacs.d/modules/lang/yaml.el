(require 'core-straight)

(package! yaml-mode
  :mode
  (("\\.yml\\'"        . yaml-mode)
   ("\\.yaml\\'"       . yaml-mode)
   ("\\.Procfile\\'"   . yaml-mode)))

(lsp! yaml-mode
  (auto-ide/add! 'yaml-mode #'hydra-lsp/body))
