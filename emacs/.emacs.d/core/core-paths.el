;;; core-paths.el --- some path logic -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'find-lisp)

(defconst shan-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defconst custom-file (concat user-emacs-directory "custom.el"))

(defvar shan-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory with all the core files.")

(defvar shan-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory with all the module files.")

(defconst shan-local-dir (expand-file-name ".local" user-emacs-directory)
  "Root directory for local storage.

TODO: figure out how to move to doom esque management from `no-littering'.")

(defconst shan-etc-dir (expand-file-name "etc" shan-local-dir)
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst shan-var-dir (expand-file-name "var" shan-local-dir)
  "Directory for installed and created files for config.")

(defconst shan-cache-dir (expand-file-name "cache" shan-var-dir)
  "Directory for volatile local storage.
Use this for files that change often, like cache files.")

(provide 'core-paths)
;;; core-paths.el ends here
