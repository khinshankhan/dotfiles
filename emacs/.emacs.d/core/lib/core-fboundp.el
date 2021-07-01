;;; core-fboundp.el --- mostly stuff that goes into the function space -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 's)

(defmacro k-time! (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defmacro with-os! (os &rest body)
  "Execute BODY if current os is OS."
  (declare (indent 1))
  `(when (if (consp ',os) (memq system-type ',os) (eq system-type ',os))
     ,@body))

(defmacro no-hook! (f hooks)
  "Call function F while temporarily removing HOOKS."
  `(lambda (&rest args)
     (let ((tbl (cl-loop for hook in ,hooks collect `(,(gensym) . ,hook))))
       (prog2
           (dolist (pair tbl)
             (eval `(setq ,(car pair) ,(cdr pair)))
             (eval `(setq ,(cdr pair) nil)))
           (apply ,f args)
         (dolist (pair tbl)
           (eval `(setq ,(cdr pair) ,(car pair))))))))

(defmacro do-once-1-sec-after-emacs-startup (&rest body)
  "Does BODY after 1 second of loaded config."
  `(run-with-idle-timer 1 ; run this after emacs is idle for 1 second
                        nil ; do this just once; don't repeat
                        (lambda () ,@body)))

;; Quality of Life a la Doom
(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defvar doom--transient-counter 0
  "Counter to help debug when hook an error may occur on.")

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.
FORMS are evaluated once, when that function/hook is first invoked, then never
again.
HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "doom--transient-%d-h" (cl-incf doom--transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'.  WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

;; Au revoir Doom...

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

(defun shan/add-list-to-list (to-list from-list &optional append compare-fn)
  "Add all elements from FROM-LIST to TO-LIST.  APPEND and COMPARE-FN work as they in `add-to-list'."
  (dolist (elem from-list)
    (add-to-list to-list elem append compare-fn))
  to-list)

(defun shan/copy-hooks-to (from-hook to-hook)
  "Copy one list of hooks to another, from FROM-HOOK into TO-HOOK.
This avoid without the weird nonc circular list problem."
  (dolist (hook from-hook)
    (add-hook to-hook hook)))

;; TODO: I seriously need to figure out proper saving...
(defun shan/vanilla-save ()
  "Save file without any hooks applied."
  (interactive)
  (funcall (no-hook! 'save-buffer '(before-save-hook after-save-hook))))

(defun shan/call-keymap (map &optional prompt)
  "Read a key sequence and call the command it's bound to in MAP.
It can use PROMPT for the key sequence."
  (let* ((help-form `(describe-bindings ,(vector map)))
         (key (read-key-sequence prompt))
         (cmd (lookup-key map key t)))
    (if (functionp cmd) (call-interactively cmd)
      (user-error "%s is undefined" key))))

(defun shan/exec-call-keymap (keymap prompt)
  "Execute `shan/call-keymap' using KEYMAP and PROMPT."
  (interactive)
  (shan/call-keymap keymap prompt))

(defun symbol-append (&rest symbols)
  "Concatenate n SYMBOLS and return a symbol."
  (intern (apply #'s-concat
                 (mapcar #'symbol-name symbols))))

(provide 'core-fboundp)
;;; core-fboundp.el ends here
