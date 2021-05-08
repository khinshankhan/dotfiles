;;; parentheses.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode))

(package! paren
  :demand t
  :config
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
  (show-paren-mode t))

(package! rainbow-delimiters
  :if (feature-p! +rainbow)
  :hook
  (prog-mode . rainbow-delimiters-mode))

(provide 'parentheses)
;;; parentheses.el ends here
