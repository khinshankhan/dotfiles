;;; init.el -*- lexical-binding: t; -*-

;; More Doom safety logic

;; In the strange case that early-init.el wasn't loaded (e.g. you're using
;; Chemacs 1? Or you're loading this file directly?), we do it explicitly:
(unless (boundp 'shan-core-dir)
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))
