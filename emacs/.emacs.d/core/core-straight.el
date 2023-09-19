;;; core-straight.el --- package manager -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; HACK: I have no idea what this is, but straight needs it 'defined'
;; I think it was renamed but there's a discrepancy between straight and emacs
(defvar native-comp-deferred-compilation-deny-list
  (bound-and-true-p native-comp-async-env-modifier-form))

(defconst straight-repository-branch "master") ; more stable branch
(defconst straight-use-package-by-default t)
(defconst straight-recipe-repositories nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

;; abstract straight so it can be observed and potentially hot swappable later
(defvar core-straight--loaded-packages '()
  "List containing loaded packages.")

(defmacro package! (name &rest args)
  "Like `use-package', but cooler since it also tracks which packages loaded.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  (add-to-list 'core-straight--loaded-packages name)
  `(use-package ,name
     ,@args))

(provide 'core-straight)
;;; core-straight.el ends here
