;;; core-lib.el --- core module that defines utility functions and macros -*- lexical-binding: t -*-
;;; Commentary:
;; Loads all other core-lib modules.
;;; Code:

(require 'core-straight)
(require 'core-paths)

;; these are just cool libraries I’d like to use during my config, or many of the packages use them

;; load this before any other third-party packages to keep init directory clean
(use-package! no-littering
  :demand t
  :init
  (require 'no-littering))

(use-package! dash
  :demand t)
(use-package! f
  :demand t)
(use-package! s
  :demand t)
(use-package! string-inflection
  :demand t)

(add-to-list 'load-path (expand-file-name "lib" shan-core-dir))

;; For the vars/ macros/ fns/ tricks n gimmicks: defines functions and macros used throughout the configuration.
;; Personal functions, some packages are reliant on these, so it goes on top. Working on credit for people not mentioned
;; in preface and significant enough. People should be cited even if the function was modified. Functions are split
;; into… "sensible" groups. Note, they’re prefixed with shan/ over other prefixes because I needed 'namespaces and I
;; like auto completing any 'custom' function off of this one prefix instead of remembering more which people may have
;; originally defined for themselves. One day I'll get to attributing everyone...

;; NOTE: Even though some of the functions leverage some macros, the order is
;; okay because it's fine to be in the body of functions. Would be an issue if
;; unless the body of the file used the macros.
(defvar shan--core-lib-modules
  '(core-boundp
    core-fboundp
    tricks-n-gimmicks
    core-module))

;; load rest of core-lib modules
(dolist (module shan--core-lib-modules)
  (require module))

(provide 'core-lib)
;;; core-lib.el ends here
