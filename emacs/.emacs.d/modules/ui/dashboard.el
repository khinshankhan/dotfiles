(require 'core-straight)

(package! dashboard
  :demand t
  :bind
  (:map dashboard-mode-map
        ("n" . widget-forward)
        ("p" . widget-backward))
  :config
  (setq dashboard-banner-logo-title "Do you ever wonder why we're always, like, wearing gloves?"
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-set-init-info t
        dashboard-set-footer nil)

  ;; (setq dashboard-set-navigator t)

  (setq dashboard-items '((recents  . 10)
                          ;; (bookmarks . 5)
                          ;; (projects . 5)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          ))

  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (if shan--settings-exist?
                                     shan--preferred-logo ;; weird stuff, possibly because of no-littering
                                   'logo))

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

  (defun shan--dashboard-set-init-info()
    (setq dashboard-init-info
	      (format "%d packages loaded in %s sec."
		          (length shan--loaded-packages)
                  (emacs-init-time))))
  (add-hook 'after-init-hook 'shan--dashboard-set-init-info))
