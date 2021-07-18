(require 'core-straight)
(require 'core-module)

(package! go-mode
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(package! gotest)

(package! go-tag
  :config
  (setq go-tag-args (list "-transform" "camelcase")))

(lsp! go-mode
  (dap!
    (require 'dap-gdb-lldb)
    (require 'dap-go)))
