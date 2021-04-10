;;; core-straight.el --- package manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst package-enable-at-startup nil)
(defconst straight-use-package-by-default t)
(defconst straight-recipe-repositories nil)
(defconst straight-repository-branch "master")
(defconst straight-fix-org nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; TODO: I still have some stray demands in my config, should figure that out sometime
(setq-default use-package-always-defer nil
	          use-package-always-demand t
	          byte-compile-warnings nil)
;; (setq use-package-verbose t)

(straight-use-package 'use-package)

;; betterify straight
(defvar shan--loaded-packages '()
  "List containing loaded packages.")

(defmacro package! (name &rest args)
  "Like `use-package', but cooler since it also tracks which packages were loaded.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  (add-to-list 'shan--loaded-packages name)
  `(use-package ,name
     ,@args))

(defmacro feature! (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  (add-to-list 'shan--loaded-packages name)
  `(use-package ,name
     :straight nil
     ,@args))

(provide 'core-straight)
;;; core-straight.el ends here
