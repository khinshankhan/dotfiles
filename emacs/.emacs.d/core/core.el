;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all other core modules.
;;; Code:

;; figure out proper pathing
(load (expand-file-name "core-paths" (expand-file-name "core" user-emacs-directory)) nil 'nomessage)

(defvar shan--core-modules
  '(core-paths
    core-straight
    core-lib
    core-gc
    core-basics))

;; load rest of core modules
(dolist (module shan--core-modules)
  (require module))

(provide 'core)
;;; core.el ends here
