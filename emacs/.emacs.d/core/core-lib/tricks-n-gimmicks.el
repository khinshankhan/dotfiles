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

;; Check git access
(defconst shan--gh-access (string-prefix-p "Hi" (shell-command-to-string "ssh -T git@github.com"))
  "Checks if Emacs has ssh access for GitHub (inherited path).")
(defconst shan--gl-access (string-prefix-p "Welcome" (shell-command-to-string "ssh -T git@gitlab.com"))
  "Checks if Emacs has ssh access for GitLab (inherited path).")

(if (and shan--gh-access shan--gl-access)
    (setq straight-vc-git-default-protocol 'ssh)
  (message "GH ACCESS: %s" shan--gh-access)
  (message "GL ACCESS: %s" shan--gl-access))

(provide 'tricks-n-gimmicks)
;;; tricks-n-gimmicks.el ends here
