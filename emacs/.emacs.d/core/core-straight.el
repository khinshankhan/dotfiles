;;; core-straight.el --- package manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; HACK: I have no idea what this is, but straight needs it 'defined'
;; I think it was renamed but there's a discrepancy between straight and emacs
(defvar native-comp-deferred-compilation-deny-list
  (bound-and-true-p native-comp-async-env-modifier-form))

(defconst package-enable-at-startup nil)
(defconst straight-use-package-by-default t)
(defconst straight-recipe-repositories nil)
(defconst straight-repository-branch "master")
;; (defconst straight-fix-org nil)

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
;; but really the config should be redone to utilize deferring logic
(setq-default use-package-always-defer nil
              use-package-always-demand t
              byte-compile-warnings nil)
;; (setq use-package-verbose t)

(require 'straight)
(straight-use-package 'use-package)
(require 'use-package)

;; betterify straight
(defvar shan--loaded-packages '()
  "List containing loaded packages.")

;; TODO: refactor use-package! and package! such that they
;; load packages and require separately like doom
(defmacro use-package! (name &rest args)
  "Like `use-package', but cooler since it also tracks which packages loaded.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(progn
     (add-to-list 'shan--loaded-packages ',name)
     (use-package ,name
       ,@args)))

(defmacro package! (name &rest args)
  "Like `use-package', but cooler since it also tracks which packages loaded.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  (add-to-list 'shan--loaded-packages name)
  `(use-package ,name
     ,@args))

(provide 'core-straight)
;;; core-straight.el ends here
