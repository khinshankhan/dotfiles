;;; python.el --- python
;;; Commentary:
;;; Code:

(require 'core-straight)

(use-package python
  :defer t
  :straight (:type built-in)
  :custom
  (python-indent 4)
  (py-split-window-on-execute t))

;; Required for MacOS, prevents newlines from being displayed as ^G
(setq python-shell-interpreter-args (if (equal system-type 'darwin)
					                    "-c \"exec('__import__(\\'readline\\')')\" -i"
				                      "-i"))

(defun igneous--activate-venv ()
  "Activates a venv."
  (interactive)
  (unless (bound-and-true-p pyvenv-virtual-env-name)
    (call-interactively #'pyvenv-activate)))

(use-package pyvenv
  :after python
  :hook
  (python-mode . igneous--activate-venv)
  (python-mode . pyvenv-mode))

(with-feature! +cython
               (use-package cython-mode)
               (use-package flycheck-cython
                 :if (module-p! :tools flycheck)
                 :after flycheck
                 :config
                 (add-to-list 'flycheck-checkers 'cython)))

(with-feature! +lsp
               (if (module-p! :tools lsp)
                   (add-hook 'python-mode-hook #'lsp 80)  ;; 80 is the depth, helping making sure it runs after other hook functions
                 (warn "Module language/python requires module tools/lsp for feature +lsp to work.")))

(with-eval-after-load 'lsp-mode
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  (setq lsp-pyls-plugins-autopep8-enabled nil
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-mccabe-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-pyflakes-enabled nil
        lsp-pyls-plugins-yapf-enabled nil))

(provide 'python)
;;; python.el ends here
