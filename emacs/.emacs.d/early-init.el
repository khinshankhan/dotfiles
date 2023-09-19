;;; early-init.el --- config entry point -*- lexical-binding: t; -*-

;;; Commentary:

;; Mostly from Doom

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;;; Code:

(require 'cl-lib)

;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make are
;;   startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; Defer garbage collection further back in the startup process
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun shan|revert-gc ()
  "Reset values and garbage collect."
  (setq gc-cons-threshold (* 16 1024 1024) ; 16 mib is better than the default
        gc-cons-percentage 0.1
        file-name-handler-alist (append last-file-name-handler-alist
                                        file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal) ; apparently we end up with duplicates
  (makunbound 'last-file-name-handler-alist)
  (garbage-collect))

(add-hook 'after-init-hook 'shan|revert-gc)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package initialization with straight,
;; so we must prevent Emacs from doing it early!
(when (>= emacs-major-version 27)
  (setq package-enable-at-startup nil))
(fset #'package--ensure-init-file #'ignore) ; DEPRECATED Removed in 28

;; Ensure config is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

;; Load the heart of config
(load (expand-file-name "core/core" user-emacs-directory)
      nil 'nomessage)

;; Load modules that were requested
(require 'core-module)
(core-module/load-config)

(provide 'early-init)
;;; early-init.el ends here
