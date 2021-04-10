;;; core-basics.el --- Core module that sets up the basic configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Contains basic configuration.
;;; Code:

(require 'core-straight)

;; encoding
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(setq-default locale-coding-system 'utf-8)
(dolist (fn '(set-terminal-coding-system set-keyboard-coding-system set-selection-coding-system prefer-coding-system))
  (if (fboundp fn)
      (funcall fn 'utf-8)))

(package! unidecode)

;; backups
(setq-default backup-inhibited t
              auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

;; weird errors of GTK without this

(when (>= emacs-major-version 26)
  (setq-default confirm-kill-processes nil))

;; you need to experience keyboard to realize keyboard master race. (`fn + f10' if need be for options though)
(setq inhibit-startup-message t)
(dolist (fn '(tool-bar-mode scroll-bar-mode menu-bar-mode))
  (if (fboundp fn)
      (funcall fn -1)))

(provide 'core-basics)
;;; core-basics.el ends here
