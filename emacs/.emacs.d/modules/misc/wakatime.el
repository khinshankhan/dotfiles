(require 'core-straight)

;;; Monitor my coding activity.
;;; Remember to set `wakatime-api-key' in personal/settings.el.
(package! wakatime-mode
  :if (and (executable-find "wakatime") (boundp 'wakatime-api-key))
  :config
  (setq wakatime-cli-path (executable-find "wakatime"))
  (global-wakatime-mode))
