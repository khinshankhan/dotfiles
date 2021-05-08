;;; core-projects.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-straight)
(require 'core-paths)

(defvar doom-projectile-cache-limit 10000
  "If any project cache surpasses this many files it is purged when quitting Emacs.")

(defvar doom-projectile-cache-blacklist '("~" "/tmp" "/")
  "Directories that should never be cached.")

(defvar doom-projectile-cache-purge-non-projects nil
  "If non-nil, non-projects are purged from the cache on `kill-emacs-hook'.")

(defun doom-project-ignored-p (project-root)
  "Return non-nil if remote or temporary file, or a straight package given PROJECT-ROOT."
  (and (not (file-remote-p project-root))
       (or (file-in-directory-p project-root temporary-file-directory)
           (file-in-directory-p project-root shan-local-dir))))

(package! projectile
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :config
  (setq projectile-cache-file (concat shan-cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching doom-interactive-p
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (f-join shan-cache-dir "projectile.projects")
        projectile-ignored-projects (list "~/" temporary-file-directory)
        projectile-ignored-project-function #'doom-project-ignored-p
        ;; ignore set up: https://www.youtube.com/watch?v=qpv9i_I4jYU
        projectile-indexing-method (if (memq system-type '(ms-dos windows-nt cygwin))
                                       'native
                                     'alien)
        projectile-require-project-root t
        projectile-sort-order 'access-time
        projectile-completion-system (if (module-p! :ui ivy)
                                         'ivy
                                       'auto))
  (projectile-mode t)

  ;; Auto-discovery on `projectile-mode' is slow and premature. Let's defer it
  ;; until it's actually needed. Also clean up non-existing projects too!
  (add-transient-hook! 'projectile-relevant-known-projects
                       (projectile-cleanup-known-projects)
                       (projectile-discover-projects-in-search-path)))

(defun doom-cleanup-project-cache-h ()
  "Purge projectile cache entries that:
a) have too many files (see `doom-projectile-cache-limit'),
b) represent blacklisted directories that are too big, change too often or are
   private.  (see `doom-projectile-cache-blacklist'),
c) are not valid projectile projects."
  (when (and (bound-and-true-p projectile-projects-cache)
             projectile-enable-caching
             doom-interactive-p)
    (setq projectile-known-projects
          (cl-remove-if #'projectile-ignored-project-p
                        projectile-known-projects))
    (projectile-cleanup-known-projects)
    (cl-loop with blacklist = (mapcar #'file-truename doom-projectile-cache-blacklist)
             for proot in (hash-table-keys projectile-projects-cache)
             if (or (not (stringp proot))
                    (>= (length (gethash proot projectile-projects-cache))
                        doom-projectile-cache-limit)
                    (member (substring proot 0 -1) blacklist)
                    (and doom-projectile-cache-purge-non-projects
                         (not (doom-project-p proot)))
                    (projectile-ignored-project-p proot))
             do (message "Removed %S from projectile cache" proot)
             and do (remhash proot projectile-projects-cache)
             and do (remhash proot projectile-projects-cache-time)
             and do (remhash proot projectile-project-type-cache))
    (projectile-serialize-cache)))

(add-hook 'kill-emacs-hook 'doom-cleanup-project-cache-h)

(package! counsel-projectile
  :if (module-p! :ui ivy)
  :after
  (counsel projectile)
  :config
  (counsel-projectile-mode t)
  (defalias 'projectile-switch-to-buffer 'counsel-projectile-switch-to-buffer)
  (defalias 'projectile-find-dir 'counsel-projectile-find-dir)
  (defalias 'projectile-find-file 'counsel-projectile-find-file)
  (defalias 'projectile-grep 'counsel-projectile-grep)
  (defalias 'projectile-switch-project 'counsel-projectile-switch-project))

(provide 'core-projects)
;;; core-projects.el ends here
