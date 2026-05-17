;; LSP: pnpm -g add dockerfile-language-server-nodejs
(require 'core-straight)

(package! dockerfile-mode
  :mode
  (("Dockerfile\\'"     . dockerfile-mode)
   ("\\.dockerfile\\'"  . dockerfile-mode)))

(package! docker-compose-mode
  :mode ("docker-compose[^/]*\\.ya?ml\\'" . docker-compose-mode))

(lsp! dockerfile-mode
  (auto-ide/add! 'dockerfile-mode #'hydra-lsp/body))
