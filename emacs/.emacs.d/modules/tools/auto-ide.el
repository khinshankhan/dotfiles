(require 'core-paths)

;; Add an hydra to a list, based on a mode, which then gets resolved by `auto-ide/resolve'.
(defvar auto-ide--alist '()
  "List containing relationships of (mode . hydra).")

(defun auto-ide/add (mode hydra)
  "Add MODE and HYDRA as (mode . hydra) to `auto-ide--alist'."
  (push `(,mode . ,hydra) auto-ide--alist))

(defun auto-ide/resolve ()
  "Call a hydra related to active mode if found in `auto-ide--alist'."
  (interactive)
  (let ((hydra (alist-get major-mode auto-ide--alist)))
    (if hydra
        (funcall hydra)
      (message "IDE not found for %s" major-mode))))
