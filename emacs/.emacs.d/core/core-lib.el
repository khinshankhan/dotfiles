;;; core-lib.el --- Core module that defines utility functions and macros -*- lexical-binding: t -*-
;;; Commentary:
;; Loads all other core-lib modules.
;;; Code:

(require 'core-paths)

(add-to-list 'load-path (expand-file-name "core-lib" shan-core-dir))

;; For the vars/ macros/ fns/ tricks n gimmicks: defines functions and macros used throughout the configuration.
;; Personal functions, some packages are reliant on these, so it goes on top. Working on credit for people not mentioned
;; in preface and significant enough. People should be cited even if the function was modified. Functions are split
;; into… "sensible" groups. Note, they’re prefixed with shan/ over other prefixes because I needed 'namespaces and I
;; like auto completing any 'custom' function off of this one prefix instead of remembering more which people may have
;; originally defined for themselves. One day I'll get to attributing everyone...

(defvar shan--core-lib-modules
  '(core-vars
    core-macros
    core-fns
    tricks-n-gimmicks
    core-module))

;; load rest of core-lib modules
(dolist (module shan--core-lib-modules)
  (require module))

(provide 'core-lib)
;;; core-lib.el ends here
