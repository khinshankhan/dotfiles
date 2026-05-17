;; LSP: github.com/elixir-lsp/elixir-ls
(require 'core-straight)

(package! elixir-mode
  :mode
  (("\\.ex\\'"  . elixir-mode)
   ("\\.exs\\'" . elixir-mode)))

(lsp! elixir-mode
  (auto-ide/add! 'elixir-mode #'hydra-lsp/body))
