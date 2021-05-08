;;; modeline.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-straight)

(dolist (fn '(line-number-mode column-number-mode))
  (if (fboundp fn)
      (funcall fn t)))

(package! doom-modeline
  :demand t
  :config
  (setq doom-modeline-python-executable "python3"
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-version t
        doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-mode))

(package! hide-mode-line
  :hook
  ((neotree-mode
    imenu-list-minor-mode
    minimap-mode ibuffer-mode
    help-mode
    deft-text-mode
    Man-mode)
   . hide-mode-line-mode))

(provide 'modeline)
;;; modeline.el ends here
