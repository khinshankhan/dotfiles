(require 'core-straight)
(require 'core-paths)

(package! keyfreq
  :config
  (setq keyfreq-file (expand-file-name "keyfreq.el" shan-cache-dir))
  (keyfreq-autosave-mode t)
  (keyfreq-mode t))
