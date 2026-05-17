;; LSP: go install github.com/grafana/jsonnet-language-server@latest
(require 'core-straight)

(package! jsonnet-mode
  :mode
  (("\\.jsonnet\\'"    . jsonnet-mode)
   ("\\.libsonnet\\'"  . jsonnet-mode)))

(lsp! jsonnet-mode
  (auto-ide/add! 'jsonnet-mode #'hydra-lsp/body))
