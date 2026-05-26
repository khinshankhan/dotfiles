;; LSP: go install golang.org/x/tools/gopls@latest
(require 'core-straight)

(defun go-custom/update-tooling ()
  "Install or update common Go tooling to latest versions."
  (interactive)
  (let ((cmd (mapconcat
              #'identity
              '("go install golang.org/x/tools/gopls@latest"
                "go install github.com/go-delve/delve/cmd/dlv@latest"
                "go install honnef.co/go/tools/cmd/staticcheck@latest"
                "go install golang.org/x/tools/cmd/goimports@latest"
                "go install github.com/fatih/gomodifytags@latest"
                "go install github.com/cweill/gotests/gotests@latest")
              " && ")))
    (async-shell-command cmd "*go-tooling-update*")))

(package! go-mode
  :mode
  (("\\.go\\'"    . go-mode)
   ("go\\.mod\\'" . go-mode)
   ("go\\.sum\\'"  . go-mode)
   ("\\.gotmpl\\'" . go-mode))
  :bind (:map go-mode-map
              ("C-c v" . go-custom/mod-vendor)
              ("C-c t u" . go-custom/update-tooling))
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
    (call-process-shell-command (concat "cd " (vc-root-dir) "; go get -u github.com/cweill/gotests/...") nil 0)
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
  ;; Some gopls versions reject this setting?
  (setq lsp-go-complete-function-calls nil)
  ;; lsp-go registers the key unconditionally for booleans, so also drop it
  ;; from lsp-mode's settings registry after the client is loaded.
  (with-eval-after-load 'lsp-go
    (when (boundp 'lsp-client-settings)
      (remhash "gopls.completeFunctionCalls" lsp-client-settings)))
  (dap!
    (require 'dap-gdb-lldb)
    (require 'dap-dlv-go))
  (auto-ide/add! 'go-mode #'hydra-lsp/body))
