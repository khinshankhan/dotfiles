;; LSP: github.com/zigtools/zls
(require 'core-straight)

(straight-register-package
 '(zig-mode :type git :host nil
            :repo "https://codeberg.org/ziglang/zig-mode.git"))

(package! zig-mode
  :mode ("\\.zig\\'" . zig-mode))

(lsp! zig-mode
  (auto-ide/add! 'zig-mode #'hydra-lsp/body))
