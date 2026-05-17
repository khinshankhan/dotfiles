;; LSP: github.com/jeapostrophe/racket-langserver
(require 'core-straight)

(package! racket-mode
  :mode ("\\.rkt\\'" . racket-mode))

(lsp! racket-mode
  (auto-ide/add! 'racket-mode #'hydra-lsp/body))
