(require 'core-straight)
(require 'core-module)

;;; Apheleia
(with-feature! +apheleia
  (defun +format-prettier-command ()
    "Return \"prettier\" if found locally in exec-path, otherwise \"apheleia-npx\"."
    (if (executable-find "prettier") "prettier" "apheleia-npx"))

  (package! apheleia
    :config
    (dolist (key '(prettier prettier-css prettier-html prettier-javascript
                   prettier-json prettier-scss prettier-svelte
                   prettier-typescript prettier-yaml))
      (when-let* ((cmd (alist-get key apheleia-formatters)))
        (setf (alist-get key apheleia-formatters)
              (cl-remove-if (lambda (x)
                              (and (listp x) (eq (car x) 'apheleia-formatters-js-indent)))
                            (if (equal (car cmd) "apheleia-npx")
                                (cons '(+format-prettier-command) (cdr cmd))
                              cmd))))))

  (defun +format-enable-apheleia ()
    "Enable apheleia-mode (idempotent, safe for derived mode hooks)."
    (apheleia-mode 1)))

(defun +format-disable-lsp-on-save ()
  "Buffer-locally disable LSP format-on-save where apheleia handles formatting."
  (setq-local lsp-before-save-edits nil))

