(require 'core-straight)

(package! go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("C-c v" . go-custom/mod-vendor))
  :config
  (defun go-custom/mod-vendor ()
    "Run 'go mod vendor' at repository root."
    (interactive)
    (progn
      (call-process-shell-command (concat "cd " (vc-root-dir) "; go mod vendor") nil 0)
      (message "Ran 'go mod vendor'!")))

  (setq go-test-verbose t
        gofmt-command "gofmt"
        gofmt-args '("-s"))
  (add-hook 'before-save-hook #'gofmt-before-save))

;; TODO: look into this
(package! go-guru)

(package! gotest
  :bind (:map go-mode-map
              ("C-c t f" . go-custom/test-clean-and-current-file)
              ("C-c t p" . go-custom/test-clean-and-current-project)
              ("C-c t i" . go-custom/setup-project-test))
  :config
  (defun go-custom/test-clean-and-current-file ()
    (interactive)
    (progn
      (call-process-shell-command "go clean -testcache" nil 0)
      (go-test-current-file)))
  (defun go-custom/test-clean-and-current-project ()
    (interactive)
    (progn
      (call-process-shell-command "go clean -testcache" nil 0)
      (go-test-current-project)))

  ;; call per project:
  (defun go-custom/setup-project-test ()
    "Call per project to get testing generation."
    (interactive)
    (call-process-shell-command (concat "cd " (vc-root-dir) "go get -u github.com/cweill/gotests/...") nil 0)
    (message "Ran 'get gotests'!"))

  (defun go-custom/setup-global-test ()
    "Call globally to get testing generation."
    (interactive)
    (call-process-shell-command "GO111MODULE=off go get -u github.com/cweill/gotests/..." nil 0)
    (message "Ran global 'get gotests'!")))

(package! go-gen-test
  :bind (:map go-mode-map
              ("C-c t d" . go-gen-test-dwim)
              ("C-c t a" . go-gen-test-all)))

;; generate and edit field tags for golang struct fields
(package! go-tag
  :config
  (setq go-tag-args (list "-transform" "camelcase")))

(package! gorepl-mode
  :commands gorepl-run-load-current-file)

(lsp! go-mode
  (dap!
    (require 'dap-gdb-lldb)
    (require 'dap-go))
  (auto-ide/add! 'go-mode #'hydra-lsp/body))
