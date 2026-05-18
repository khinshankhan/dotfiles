;;; tricks-n-gimmicks.el --- general nice configuration to have, it’ll help with later in the config -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads all my tricks-n-gimmicks for a sweeter config.
;;; Code:
(require 'core-straight)
(require 'core-util)
(require 'core-paths)

;; ensure our config directories exist
;; since some programs will fail like `recentf' or `keyfreq'
(dolist (dir (list shan-local-dir shan-etc-dir shan-var-dir shan-cache-dir))
  (f-mkdir dir))

;; setup proper paths for *nix machines
;; Async shell PATH loading to avoid blocking startup.
(with-os! (gnu/linux darwin)
  (defun shan--async-load-path ()
    "Load PATH from shell asynchronously."
    (let ((shell (getenv "SHELL"))
          (buf (generate-new-buffer " *path-cache*")))
      (set-process-sentinel
       (start-process "path-cache" buf shell "-l" "-i" "-c"
                      "printf '__PATH__\\000%s\\000__PATH__' \"$PATH\"")
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (with-current-buffer (process-buffer proc)
             (goto-char (point-min))
             (when (re-search-forward "__PATH__\0\\(.*?\\)\0__PATH__" nil t)
               (let ((path (match-string 1)))
                 (setenv "PATH" path)
                 (setq exec-path (append (parse-colon-path path)
                                         (list exec-directory))))))
           (kill-buffer (process-buffer proc)))))))

  (defun shan/shell-sync ()
    "Sync PATH from shell."
    (interactive)
    (message "Syncing PATH from shell...")
    (shan--async-load-path))

  (add-hook 'emacs-startup-hook #'shan--async-load-path))

(setq straight-vc-git-default-protocol 'ssh)

;; custom settings
(defvar shan--preferred-logo 'logo
  "Preferred logo for dashboard startup.  If not found, use default.")

(defconst shan--settings-path
  (expand-file-name "settings.el"
                    (expand-file-name "personal" user-emacs-directory))
  "Path to personal settings meant not be public (api keys and stuff).")

(defconst shan--settings-exist? (file-exists-p shan--settings-path)
  "Checks if shan--settings-path exists.")

(if shan--settings-exist?
    (load shan--settings-path nil 'nomessage)
  (message "Settings file not found!"))

;; From doom: these two functions don't exist in terminal Emacs, but some Emacs
;; packages (internal and external) use it anyway, leading to void-function
;; errors. I define a no-op substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

(after! whitespace
  (defun doom-is-childframes-p ()
    "`whitespace-mode' inundates child frames with whitespace markers, so
disable it to fix all that visual noise."
    (null (frame-parameter nil 'parent-frame)))
  (add-function :before-while whitespace-enable-predicate #'doom-is-childframes-p))

;;; Extra file extensions to support
(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

;;; Multiple languages can be useful when dealing with templates
(package! mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 0))

(provide 'tricks-n-gimmicks)
;;; tricks-n-gimmicks.el ends here
