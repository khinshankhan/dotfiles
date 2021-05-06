;;; theme.el --- theme
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! doom-themes
  :demand t
  :config
  (setq doom-vibrant-brighter-comments t
        doom-vibrant-brighter-modeline t)
  (doom-themes-org-config)
  (load-theme 'doom-dracula t))

(package! solaire-mode
  :demand t
  :functions persp-load-state-from-file
  :hook
  (prog-mode . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  (after-load-theme . solaire-mode-swap-bg)
  :config
  (setq solaire-mode-remap-modeline nil
        solaire-mode-remap-fringe nil)
  (solaire-global-mode 1)
  (solaire-mode-swap-bg)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))

(provide 'theme)
;;; theme.el ends here
