;;; julia.el --- -*- lexical-binding: t; -*-
;; LSP: github.com/julia-vscode/LanguageServer.jl
(require 'core-straight)

(package! julia-mode
  :mode ("\\.jl\\'" . julia-mode))

(lsp! julia-mode
  (auto-ide/add! 'julia-mode #'hydra-lsp/body))
