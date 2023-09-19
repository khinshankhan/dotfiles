(require 'core-straight)
(require 'core-paths)

(defvar lsp-custom--ignore-alist '())

(defun lsp-custom/ignore-mode (mode)
  "Ignore `mode' for lsp activation."
  (interactive)
  (add-to-list 'lsp-custom--ignore-alist mode))

(defun lsp-custom/activate-lsp ()
  (unless (-contains? lsp-custom--ignore-alist major-mode)
    ;; trust that modules/tools/lsp will be activated
    ;; by the time files open
    (lsp)))

(package! lsp-mode
  :commands lsp-install-server
  :bind
  (:map lsp-mode-map
        ([remap xref-find-definitions] . lsp-find-definition)
        ("C-c e" . lsp-rename)
        ("C-c f" . lsp-format-buffer))
  :config
  ;; Handle paths for lsp related generated files
  (setq lsp-session-file (expand-file-name "lsp-session" shan-etc-dir)
        lsp-server-install-dir (expand-file-name "lsp" shan-etc-dir))

  (setq lsp-auto-configure t ; this does a lot of magic we have to work around
        lsp-auto-guess-root t
        lsp-enable-snippet nil
        lsp-keep-workspace-alive nil
        lsp-before-save-edits t
        lsp-enable-indentation t
        lsp-prefer-capf t
        ;; TODO: should probably look into cases where this is too high
        ;; but the default 1000 is too low
        lsp-file-watch-threshold 500000
        ;; supposedly features that have great potential to be slow
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        ;; maybe if I spent time customizing... modeline is good enough
        lsp-headerline-breadcrumb-enable nil
        ;; TODO: make this a debug variable?
        lsp-print-io nil
        ;; lsp-modeline-diagnostics-enable nil
        ;; I think they actually got rid of this because no one uses flymake...
        lsp-prefer-flymake nil))

;; https://emacs.stackexchange.com/a/68951
(add-hook 'lsp-after-apply-edits-hook
          (lambda (operation)
            (when (eq operation 'rename)
              (save-buffer))))

;;; Great for debugging... once you learn how to use a debugger...
(package! dap-mode
  :if (feature-p! +dap)
  :config
  (setq dap-breakpoints-file (expand-file-name "dap/breakpoints" shan-etc-dir)
        dap-utils-extension-path (expand-file-name "dap/extensions" shan-etc-dir)))

;; TODO: will look into this later
;; definitely potential for optimization
(package! lsp-ui
  :if (feature-p! +ui)
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-mode-map
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ([remap flycheck-list-errors] . lsp-ui-flycheck-list))
  :config
  (setq lsp-ui-doc-border (face-foreground 'default)
        lsp-ui-flycheck-enable t
        lsp-ui-doc-include-signature t)

  (with-eval-after-load 'hl-line
    (set-face-background 'lsp-ui-doc-background (face-background 'hl-line))))
