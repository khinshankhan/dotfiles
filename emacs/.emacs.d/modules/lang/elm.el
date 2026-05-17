;; LSP: pnpm -g add @elm-tooling/elm-language-server
(require 'core-straight)

(package! elm-mode
  :mode ("\\.elm\\'" . elm-mode))

(lsp! elm-mode
  (auto-ide/add! 'elm-mode #'hydra-lsp/body))
