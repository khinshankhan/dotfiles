;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all other core modules.
;;; Code:

;; figure out proper pathing
(load (expand-file-name (expand-file-name "core/core-paths" user-emacs-directory))
      nil 'nomessage)
(require 'core-paths)
(add-to-list 'load-path shan-core-dir)

;; Nix-installed binaries must be visible before modules load.
(when (file-directory-p "~/.nix-profile/bin")
  (let ((nix-bin (expand-file-name "~/.nix-profile/bin")))
    (add-to-list 'exec-path nix-bin)
    (setenv "PATH" (concat nix-bin ":" (getenv "PATH")))))

;; set up package management
(require 'core-paths)
(require 'core-straight)

; load this before any other third-party packages to keep init directory clean
(package! no-littering
  :demand t
  :init
  (require 'no-littering))

;;; extremely helpful for figuring out what went wrong with the config file
;;; also, it's helpful for writing packages
(package! bug-hunter :demand t)

;; these are just cool libraries I’d like to use during my config, or many of the packages use them
(package! dash :demand t)
(package! f :demand t)
(package! s :demand t)
(package! string-inflection :demand t)

;; load core up
(dolist (core-module '(core-util
                       tricks-n-gimmicks
                       core-module
                       core-gc
                       core-basics
                       core-projects))
  (require core-module))

(provide 'core)
;;; core.el ends here
