;;; core-fns.el -*- lexical-binding: t -*-

(require 'core-macros)

;; these are helpful for binding
(defun shan/do-nothing ()
  "Do nothing."
  (interactive)
  nil)

(defun shan/before (to-call-before f)
  "Run TO-CALL-BEFORE then run F."
  (funcall to-call-before)
  (funcall f))

(defun shan/after (to-call-after f)
  "Run F then run TO-CALL-AFTER."
  (funcall f)
  (funcall to-call-after))

;; buffer related functions
(defun shan/refresh-buffer ()
  "Refresh the current buffer."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun shan/scratch ()
  "Create a new scratch buffer to work in.  (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0) bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

;; file/ buffer magic functions
(defun shan/sudo-edit (file-name)
  "Like find file, but opens FILE-NAME as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun shan/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun shan/rename-this-file-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let ((new-name (read-string
                   "New name:"
                   (file-name-nondirectory (buffer-file-name))))
        (name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun shan/browser-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun shan/path-copy ()
  "Copy the current file path to kill ring."
  (interactive)
  (kill-new buffer-file-name))

;; non interactive ease of config
(defun shan/add-list-to-list (to-list from-list &optional append compare-fn)
  "Add all elements from FROM-LIST to TO-LIST.  APPEND and COMPARE-FN work as they in `add-to-list'."
  (dolist (elem from-list)
    (add-to-list to-list elem append compare-fn))
  to-list)

(defun shan/copy-hooks-to (from-hook to-hook)
  "Copies one list of hooks to another, without the weird nonc circular list problem"
  (dolist (hook from-hook)
    (add-hook to-hook hook)))

;; TODO: I seriously need to figure out proper saving...
(defun shan/vanilla-save ()
  "Save file without any hooks applied."
  (interactive)
  (funcall (no-hook! 'save-buffer '(before-save-hook after-save-hook))))

;; TODO: fix this after migration org -> el
(defun shan/edit-config ()
  "Edit the configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "config.org")))

;; supposedly I can let go of `C-x' and `M-x' with these
(defun shan/call-keymap (map &optional prompt)
  "Read a key sequence and call the command it's bound to in MAP."
  (let* ((help-form `(describe-bindings ,(vector map)))
         (key (read-key-sequence prompt))
         (cmd (lookup-key map key t)))
    (if (functionp cmd) (call-interactively cmd)
      (user-error "%s is undefined" key))))

(defun shan/exec-call-keymap (keymap prompt)
  "Executes `shan/call-keymap'"
  (interactive)
  (shan/call-keymap keymap prompt))

(provide 'core-fns)
;;; core-fns.el ends here
