;;; core-projects.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-straight)
(require 'core-paths)
(require 'core-module)

(package! projectile
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" shan-cache-dir)
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching shan-interactive-p
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (f-join shan-cache-dir "projectile.projects")
        projectile-ignored-projects (list "~/" temporary-file-directory)
        ;; ignore set up: https://www.youtube.com/watch?v=qpv9i_I4jYU
        projectile-indexing-method (if (memq system-type '(ms-dos windows-nt cygwin))
                                       'native
                                     'alien)
        projectile-sort-order 'access-time
        ;; projectile-completion-system (if (module-p! :completion ivy)
        ;;                                  'ivy
        ;;                                'auto)
        projectile-require-project-root t)
  (projectile-mode t))

(package! editorconfig
  :hook
  ((prog-mode text-mode) . editorconfig-mode)
  :config
  (editorconfig-mode 1)

  ;; Fix #5057 archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are zipped XML
  ;; formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))

(provide 'core-projects)
;;; core-projects.el ends here
