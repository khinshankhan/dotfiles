;;; init.el --- possible config entry point -*- lexical-binding: t; -*-

;;; Commentary:

;; More Doom safety logic

;;; Code:

;; In the strange case that early-init.el wasn't loaded (e.g. you're using
;; Chemacs 1? Or you're loading this file directly?), we do it explicitly:
(unless (boundp 'shan-core-dir)
  (load (concat
         (if load-file-name
             (file-name-directory load-file-name)
           user-emacs-directory)
         "early-init")
        nil t))

(provide 'init)
;;; init.el ends here
