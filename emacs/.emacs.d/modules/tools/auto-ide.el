;; Add an hydra to a list, based on a mode, which then gets resolved by `shan--ide-resolve'.

(defvar shan--ide-alist '()
  "List containing relationships of (mode . hydra).")

(defun shan--ide-add (mode hydra)
  "Add MODE and HYDRA as (mode . hydra) to `shan--ide-alist'."
  (push `(,mode . ,hydra) shan--ide-alist))

(defun shan--ide-resolve ()
  "Call a hydra related to active mode if found in `shan--ide-alist'."
  (interactive)
  (let ((hydra (alist-get major-mode shan--ide-alist)))
    (if hydra
        (funcall hydra)
      (message "IDE not found for %s" major-mode))))
