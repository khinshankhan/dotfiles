;;; go.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! go-mode
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(package! gotest)

(package! go-tag
  :config
  (setq go-tag-args (list "-transform" "camelcase")))

(with-module! :tools lsp
  (with-module-feature! :tools lsp +dap
    (require 'dap-gdb-lldb)
    (require 'dap-go))

  (add-hook 'go-mode-hook #'lsp))

(provide 'go)
;;; go.el ends here
