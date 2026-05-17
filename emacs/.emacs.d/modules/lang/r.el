;; LSP: install.packages("languageserver") from R
(require 'core-straight)

(package! ess
  :mode
  (("\\.[rR]\\'"    . R-mode)
   ("\\.Rmd\\'"     . R-mode)
   ("\\.Rprofile\\'" . R-mode)))

(lsp! R-mode
  (auto-ide/add! 'R-mode #'hydra-lsp/body))
