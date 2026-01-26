(require 'core-straight)

(package! rustic
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  :config
  (setq rustic-format-on-save nil))

(lsp! rust-mode
  (dap!
    (require 'dap-gdb))
  (auto-ide/add! 'rust-mode #'hydra-lsp/body))
