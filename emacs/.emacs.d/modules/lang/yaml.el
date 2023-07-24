(require 'core-straight)

(package! yaml-mode
  :mode
  (("\\.yml\\'"        . yaml-mode)
   ("\\.yaml\\'"       . yaml-mode)
   ("\\.Procfile\\'"   . yaml-mode)))

(lsp! yaml-mode
  (shan--ide-add 'yaml-mode #'hydra-lsp/body))
