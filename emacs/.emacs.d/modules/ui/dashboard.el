(require 'core-straight)

(package! all-the-icons
  :config
  (defconst all-the-icons-font-dir (cl-case window-system
                                     (x  (concat (or (getenv "XDG_DATA_HOME")                  ;; Default Linux install directories
                                                     (concat (getenv "HOME") "/.local/share"))
                                                 "/fonts/"))
                                     (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                                     (ns (concat (getenv "HOME") "/Library/Fonts/" )))         ;; Default MacOS install directory
    "Directory where all-the-icons .tff files will install into.")
  (when (not (and (stringp all-the-icons-font-dir)
                  (--all?
                   (f-exists? (f-join all-the-icons-font-dir it))
                   all-the-icons-font-names)))
    (message "Seems some of the icons are missing from all the icons.")
    (all-the-icons-install-fonts)))

(package! dashboard
  :demand t
  :bind
  (:map dashboard-mode-map
        ("n" . widget-forward)
        ("p" . widget-backward)
        ("f" . shan/elfeed-update-database))
  :config
  (setq dashboard-banner-logo-title "Do you ever wonder why we're always, like, wearing gloves?"
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-set-init-info t
        dashboard-set-footer nil)

  ;; (setq dashboard-set-navigator t)

  (setq dashboard-items '((recents  . 5)
                          ;; (bookmarks . 5)
                          ;; (projects . 5)
                          (agenda . 5)
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
