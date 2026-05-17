;; LSP: nix profile install nixpkgs#nil
(require 'core-straight)

(package! nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(lsp! nix-mode
  (auto-ide/add! 'nix-mode #'hydra-lsp/body))
