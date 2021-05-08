;;; core-paths.el --- some path logic -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'find-lisp)

(defvar shan-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory with all the core files.")
(defvar shan-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory with all the module files.")

(add-to-list 'load-path shan-core-dir)

(provide 'core-paths)
;;; core-paths.el ends here
