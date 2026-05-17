;; LSP: pnpm -g add solidity-language-server
(require 'core-straight)

(package! solidity-mode
  :mode ("\\.sol\\'" . solidity-mode))

(lsp! solidity-mode
  (auto-ide/add! 'solidity-mode #'hydra-lsp/body))
