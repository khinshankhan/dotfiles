;; LSP: go install github.com/bufbuild/buf-language-server/cmd/bufls@latest
(require 'core-straight)

(package! protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(lsp! protobuf-mode
  (auto-ide/add! 'protobuf-mode #'hydra-lsp/body))
