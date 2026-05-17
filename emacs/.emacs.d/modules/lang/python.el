;; LSP: pip install python-lsp-server
(require 'core-straight)
(require 'core-module)

(package! python
  :defer t
  :ensure nil
  :custom
  (python-indent 4)
  (py-split-window-on-execute t)
  :config
  (when (and (equal python-shell-interpreter "python")
             (executable-find "python3"))
    (setq python-shell-interpreter "python3")
    ;; Python 3.13+ workaround for pyrepl SIGINT mishandling under comint
    (setenv "PYTHON_BASIC_REPL" "1"))

  ;; Required for MacOS, prevents newlines from being displayed as ^G
  (setq python-shell-interpreter-args (if (equal system-type 'darwin)
                                          "-c \"exec('__import__(\\'readline\\')')\" -i"
                                        "-i")))

(with-eval-after-load 'projectile
  (pushnew! projectile-project-root-files "pyproject.toml" "setup.py" "requirements.txt"))

;; inspired by https://github.com/jchedal-anglay/emacs.d/blob/rewrite/modules/language/python.el
(defun +python-activate-venv ()
  "Prompt to activate a virtualenv if one isn't active."
  (interactive)
  (unless (bound-and-true-p pyvenv-virtual-env-name)
    (call-interactively #'pyvenv-activate)))

(package! pyvenv
  :after python
  :hook
  (python-mode . +python-activate-venv)
  (python-mode . pyvenv-mode))

(with-feature! +cython
  (package! cython-mode)
  (package! flycheck-cython
    :if (core-module/feature-p :checkers 'syntax '+flycheck)
    :after flycheck
    :config
    (add-to-list 'flycheck-checkers 'cython)))

(lsp! python-mode
  (package! lsp-pyright
    :after lsp-mode)
  (auto-ide/add! 'python-mode #'hydra-lsp/body))

(dap!
  (require 'dap-python))
