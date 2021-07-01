(require 'core-straight)

(package! default-text-scale
  :if (feature-p! +text)
  :init
  (default-text-scale-mode))

(package! zoom-window
  :if (feature-p! +window)
  :bind
  ("C-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "#412170"))
