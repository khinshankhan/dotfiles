(require 'core-straight)

(setq frame-background-mode 'dark)

;; Doom themes are best esp since I use Doom modeline. Also, Doom Dracula just
;; has better keyword support as far as I’ve seen. It’s also just easy on my
;; eyes.
(package! doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)
  (load-theme 'doom-dracula t))

;; The dark nights sometimes need a little sun. The slight brightness is nice
;; for the eyes.
(package! solaire-mode
  :if (feature-p! +solaire)
  :when (or (daemonp) (display-graphic-p))
  :demand t
  :functions persp-load-state-from-file
  :hook
  (minibuffer-setup . turn-off-solaire-mode)
  :config
  (setq solaire-mode-remap-modeline nil
        solaire-mode-remap-fringe nil)
  (solaire-global-mode 1)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))
