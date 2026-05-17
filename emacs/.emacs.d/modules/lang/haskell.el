;; LSP: ghcup install hls
(require 'core-straight)

(package! haskell-mode
  :mode
  (("\\.hs\\'"    . haskell-mode)
   ("\\.lhs\\'"   . haskell-literate-mode)
   ("\\.cabal\\'" . haskell-cabal-mode)))

(lsp! haskell-mode
  (auto-ide/add! 'haskell-mode #'hydra-lsp/body))
