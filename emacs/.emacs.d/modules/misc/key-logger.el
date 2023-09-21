(require 'core-straight)
(require 'core-paths)

(package! keyfreq
  :if (feature-p! +freq)
  :config
  (setq keyfreq-file (expand-file-name "keyfreq.el" shan-cache-dir))
  (keyfreq-autosave-mode t)
  (keyfreq-mode t))

(package! command-log-mode
  :if (feature-p! +commands))
