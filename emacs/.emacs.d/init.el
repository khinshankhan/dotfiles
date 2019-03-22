;;; package --- Summary
;;; Commentary:
;; (package-initialize)
;;; Code:
; refer to https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)
; load config org file
(org-babel-load-file (concat user-emacs-directory "myinit.org"))
