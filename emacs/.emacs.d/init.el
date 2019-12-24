;; refer to https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)

;; load config org file
(let ((file-name-handler-alist nil))
  (org-babel-load-file (concat user-emacs-directory "config.org")))
