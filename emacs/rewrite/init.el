;;; init.el --- starting point of the config -*- lexical-binding: t; -*-
;;; Commentary:

;; Heavily inspired by Doom Emacs
;;; License: MIT
;;; Code:

;; we don't need garbage collection so low (maybe on a machine from a couple decade ago...)
;; nor does `load-file' need the file name handler during start up
(eval-and-compile
  (defun shan|revert-gc ()
    "Reset values."
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1)

    ;; copy values without any duplicate values
    (dolist (handler last--file-name-handler-alist)
      (add-to-list 'file-name-handler-alist handler))

    (makunbound 'last--file-name-handler-alist))

  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        last--file-name-handler-alist file-name-handler-alist
        file-name-handler-alist nil)

  (add-hook 'after-init-hook 'shan|revert-gc))

(provide 'init)
;;; init.el ends here
