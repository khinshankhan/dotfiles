;;; tricks-n-gimmicks.el --- general nice configuration to have, it’ll help with later in the config -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all my tricks-n-gimmicks for a sweeter config.
;;; Code:
(require 'core-straight)
(require 'core-fboundp)
(require 'core-paths)

;;; Extremely helpful for figuring out what went wrong with the config file.
;;; Also, it's helpful for writing packages.
(use-package! bug-hunter)

;; ensure our config directories exist
;; since some programs will fail like `recentf' or `keyfreq'
(dolist (dir (list shan-local-dir shan-etc-dir shan-var-dir shan-cache-dir))
  (f-mkdir dir))

(with-os! (gnu/linux darwin)
  (use-package! exec-path-from-shell
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
