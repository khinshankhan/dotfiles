(require 'core-straight)

(straight-register-package
 '(lean4-mode :type git :host github :repo "leanprover-community/lean4-mode"))

(package! lean4-mode
  :mode ("\\.lean\\'" . lean4-mode))
