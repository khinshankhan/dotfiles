;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all other core modules.
;;; Code:

;; figure out proper pathing
(load (expand-file-name (expand-file-name "core/core-paths" user-emacs-directory))
      nil 'nomessage)
(require 'core-paths)
(add-to-list 'load-path shan-core-dir)

(defvar shan--core-modules
  '(core-paths
    core-straight
    core-lib
    core-gc
    core-basics
    core-projects))

;; load rest of core modules
(dolist (module shan--core-modules)
  (require module))

(provide 'core)
;;; core.el ends here
