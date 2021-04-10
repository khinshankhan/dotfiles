;;; tricks-n-gimmicks.el --- general nice configuration to have, it’ll help with later in the config -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all my tricks-n-gimmicks for a sweeter config.
;;; Code:

(require 'core-straight)
(require 'core-macros)

;; load this before any other third-party packages to keep init directory clean.
(package! no-littering
  :init
  (require 'no-littering))

;; these are just cool libraries I’d like to use during my configuration, or many of the packages use them
(package! dash
  :demand t)
(package! f
  :demand t)
(package! s
  :demand t)
(package! string-inflection
  :demand t)

(require 'loadhist)
(require 'cl-seq)

(with-os! (gnu/linux darwin)
  (package! exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(provide 'tricks-n-gimmicks)
;;; tricks-n-gimmicks.el ends here
