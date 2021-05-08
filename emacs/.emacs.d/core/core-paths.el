;;; core-paths.el --- some path logic -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'find-lisp)

(defconst doom-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defvar shan-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory with all the core files.")
(defvar shan-modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory with all the module files.")

(defconst shan-local-dir (expand-file-name ".local" user-emacs-directory)
  "TODO: figure out how to move to doom esque management from `no-littering'.")

(defconst shan-var-dir (expand-file-name "var" user-emacs-directory)
  "Directory for installed and created files for config.")

(defconst shan-cache-dir (expand-file-name "shan-cache" shan-var-dir)
  "Directory for volatile local storage.
Use this for files that change often, like cache files.")

(add-to-list 'load-path shan-core-dir)

(provide 'core-paths)
;;; core-paths.el ends here
