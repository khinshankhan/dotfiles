;;; core-vars.el -*- lexical-binding: t -*-

(defconst custom-file (concat user-emacs-directory "custom.el"))
(defconst shan--settings-path (concat user-emacs-directory "personal/settings.el")
  "Path to personal settings meant not be public (api keys and stuff).")
(defconst shan--settings-exist? (file-exists-p shan--settings-path)
  "Checks if shan--settings-path exists.")

(if shan--settings-exist?
    (load-file shan--settings-path)
  (message "Settings file not found!"))

(defconst shan--preferred-logo (concat user-emacs-directory "personal/nezuko-emacs.png")
  "Preferred logo for dashboard startup. If not found, use default.")

(provide 'core-vars)
;;; core-vars.el ends here
