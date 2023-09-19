(require 'core-straight)

(dolist (fn '(line-number-mode column-number-mode))
  (if (fboundp fn)
      (funcall fn t)))

(package! doom-modeline
  :demand t
  :config
  (setq doom-modeline-support-imenu t
        doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-hud nil
        doom-modeline-window-width-limit 120
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-env-version t
        doom-modeline-python-executable "python3")
  (doom-modeline-mode t))

(package! hide-mode-line
  :hook
  ((neotree-mode
    imenu-list-minor-mode
    minimap-mode ibuffer-mode
    help-mode
    deft-text-mode
    Man-mode)
   . hide-mode-line-mode))
