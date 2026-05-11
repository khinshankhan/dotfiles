(require 'core-straight)
(require 'core-module)

;;; Apheleia
(with-feature! +apheleia
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
                                (cons "prettier" (cddr cmd))
                              cmd))))))

  (defun +format-enable-apheleia ()
    "Enable apheleia-mode (idempotent, safe for derived mode hooks)."
    (apheleia-mode 1))

  ;; prettierd formatters — available for per-project .dir-locals.el overrides
  (with-eval-after-load 'apheleia
    (dolist (entry '((prettierd-css        . ("prettierd" "--stdin-filepath" filepath "--parser=css"))
                     (prettierd-html       . ("prettierd" "--stdin-filepath" filepath "--parser=html"))
                     (prettierd-javascript . ("prettierd" "--stdin-filepath" filepath "--parser=babel-flow"))
                     (prettierd-json       . ("prettierd" "--stdin-filepath" filepath "--parser=json"))
                     (prettierd-scss       . ("prettierd" "--stdin-filepath" filepath "--parser=scss"))
                     (prettierd-svelte     . ("prettierd" "--stdin-filepath" filepath "--parser=svelte"))
                     (prettierd-typescript . ("prettierd" "--stdin-filepath" filepath "--parser=typescript"))
                     (prettierd-yaml       . ("prettierd" "--stdin-filepath" filepath "--parser=yaml"))))
      (setf (alist-get (car entry) apheleia-formatters) (cdr entry)))))

(defun +format-disable-lsp-on-save ()
  "Buffer-locally disable LSP format-on-save where apheleia handles formatting."
  (setq-local lsp-before-save-edits nil))

