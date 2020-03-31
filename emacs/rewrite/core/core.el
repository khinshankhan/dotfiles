;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(message "hello!")

;; (defun shan*load-directory-recursively(dir)
;;   "Recursively load all el files from DIR."
;;   (dolist (file (directory-files-recursively dir "\\.el$"))
;;     (load file nil 'nomessage)))

;; (defvar shan--config-dirs '("modules"))

;; (dolist (path shan--config-dirs)
;;   (shan*load-directory-recursively (shan*config-path-expand path)))


(load (shan*config-path-expand "core" "hello")
      nil 'nomessage)
(provide 'core)
;;; core.el ends here
