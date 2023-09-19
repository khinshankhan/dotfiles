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
  (prog-mode . rainbow-delimiters-mode)
  :config
  ;; I'm small-brained enough to only need 4 and get the perf boost
  (setq rainbow-delimiters-max-face-count 4))

(package! smartparens)
