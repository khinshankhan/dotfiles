;;; core-vars.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst custom-file (concat user-emacs-directory "custom.el"))
(defconst shan--settings-path
  (expand-file-name "settings.el"
                    (expand-file-name "personal" user-emacs-directory))
  "Path to personal settings meant not be public (api keys and stuff).")
(defconst shan--settings-exist? (file-exists-p shan--settings-path)
  "Checks if shan--settings-path exists.")

(if shan--settings-exist?
    (load shan--settings-path nil 'nomessage)
  (message "Settings file not found!"))

(defconst shan--preferred-logo
  (expand-file-name "nezuko-emacs.png"
                    (expand-file-name "personal" user-emacs-directory))
  "Preferred logo for dashboard startup.  If not found, use default.")

(provide 'core-vars)
;;; core-vars.el ends here
