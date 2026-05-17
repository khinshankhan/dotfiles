;; LSP: github.com/scalameta/metals
(require 'core-straight)

(package! scala-mode
  :mode
  (("\\.scala\\'" . scala-mode)
   ("\\.sc\\'"    . scala-mode)
   ("\\.sbt\\'"   . scala-mode)))

(lsp! scala-mode
  (auto-ide/add! 'scala-mode #'hydra-lsp/body))
