;; LSP: sourcekit-lsp (bundled with Xcode)
(require 'core-straight)

(package! swift-mode
  :mode ("\\.swift\\'" . swift-mode))

(lsp! swift-mode
  (auto-ide/add! 'swift-mode #'hydra-lsp/body))
