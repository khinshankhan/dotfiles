;; LSP: pnpm -g add purescript-language-server
(require 'core-straight)

(package! purescript-mode
  :mode ("\\.purs\\'" . purescript-mode))

(lsp! purescript-mode
  (auto-ide/add! 'purescript-mode #'hydra-lsp/body))
