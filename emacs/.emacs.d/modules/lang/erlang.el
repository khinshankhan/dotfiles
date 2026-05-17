;; LSP: github.com/erlang-ls/erlang_ls
(require 'core-straight)

(package! erlang
  :mode
  (("\\.erl\\'" . erlang-mode)
   ("\\.hrl\\'" . erlang-mode)))

(lsp! erlang-mode
  (auto-ide/add! 'erlang-mode #'hydra-lsp/body))
