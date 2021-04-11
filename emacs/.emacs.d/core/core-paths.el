;;; core-paths.el --- some path logic -*- lexical-binding: t; -*-

(require 'find-lisp)

(defvar shan-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory with all the core files.")
(defvar shan-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory with all the module files.")

(add-to-list 'load-path shan-core-dir)

(defconst shan--module-paths '()
  "List of all paths in `modules' directory to an elisp file.")

(mapc (lambda (filename)
        (push filename shan--module-paths))
      (find-lisp-find-files shan-modules-dir "\\.el$"))

(provide 'core-paths)
;;; core-paths.el ends here
