(require 'core-straight)
(require 'core-module)

(use-package lsp-mode
  :bind
  (:map lsp-mode-map
        ([remap xref-find-definitions] . lsp-find-definition)
        ("C-c e" . lsp-rename)
        ("C-c f" . lsp-format-buffer))
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-prefer-capf t)
  (lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-enable-diagnostics nil))

;; Hotfixes for lsp-headerline because ugh
(with-eval-after-load 'lsp-headerline
  (defun lsp-headerline--build-file-string ()
    "Build the file-segment string for the breadcrumb."
    (let* ((file-path (buffer-file-name))
           (filename (f-filename file-path))
           (file-ext (f-ext file-path))
           (icons (lsp-icons-get-by-file-ext file-ext 'headerline-breadcrumb)))
      (cond ((not file-ext) filename)
            ((not icons)
             (propertize filename 'font-lock-face (lsp-headerline--face-for-path file-path)))
            (t (concat icons " " (propertize filename 'font-lock-face (lsp-headerline--face-for-path
                                                                       file-path)))))))

  (lsp-defun lsp-headerline--symbol-icon ((&DocumentSymbol :kind))
    "Build the SYMBOL icon for headerline breadcrumb."
    (if-let (icons (lsp-icons-get-by-symbol-kind kind 'headerline-breadcrumb))
        (concat icons " "))))

(use-package lsp-ui
  :if (feature-p! +ui)
  :after lsp-mode
  :bind
  (:map lsp-mode-map
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ([remap flycheck-list-errors] . lsp-ui-flycheck-list))
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-include-signature t)
  :config
  (with-eval-after-load 'hl-line
    (set-face-background 'lsp-ui-doc-background (face-background 'hl-line))))

(with-module! :editor company
              (add-hook 'lsp-mode-hook #'company-mode))
