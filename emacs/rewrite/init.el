;;; init.el --- starting point of the config -*- lexical-binding: t; -*-
;;; Commentary:

;;; License: MIT
;;; Code:

;; we don't need garbage collection so low (maybe on a machine from a couple decade ago...)
;; nor does `load-file' need the file name handler during start up
(eval-and-compile
  (defun shan|revert-gc ()
    "Reset values."
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1)

    ;; copy values without any duplicate
    (dolist (handler last--file-name-handler-alist)
      (add-to-list 'file-name-handler-alist handler))

    (makunbound 'last--file-name-handler-alist))

  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        last--file-name-handler-alist file-name-handler-alist
        file-name-handler-alist nil)

  (add-hook 'after-init-hook 'shan|revert-gc))

;;;###autoload
(defconst shan--emacs-dir (file-name-directory load-file-name)
  "Directory where `init.el' that's being loaded is located.")

(defconst custom-file (expand-file-name "custom.el" shan--emacs-dir))

;;;###autoload
(defun shan*config-path-expand (dir &optional file)
  "Expands directory from DIR using Emacs init path.
It appends the optional FILE argument."
  (if file (expand-file-name file (expand-file-name dir shan--emacs-dir))
    (expand-file-name dir shan--emacs-dir)))

(load (shan*config-path-expand "core" "core")
      nil 'nomessage)

(provide 'init)
;;; init.el ends here
