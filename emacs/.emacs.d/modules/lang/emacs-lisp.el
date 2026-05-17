(require 'core-straight)

(package! elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(package! highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(package! macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))
