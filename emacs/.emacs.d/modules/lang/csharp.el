;; LSP: github.com/OmniSharp/omnisharp-roslyn

(lsp! csharp-mode
  (auto-ide/add! 'csharp-mode #'hydra-lsp/body))
