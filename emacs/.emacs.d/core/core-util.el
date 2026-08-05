;;; core-util.el --- defines utility variables, functions, and macros -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'cl-lib)

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

(defmacro do-once-n-sec-after-emacs-startup! (n &rest body)
  "Does BODY after 1 second of loaded config."
  `(run-with-idle-timer ,n ; run this after emacs is idle for 1 second
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

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.
PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:
- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.
This is a wrapper around `eval-after-load' that:
1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                    (require package nil 'noerror))
                #'progn
              #'with-no-warnings)
            ;; We intentionally avoid `with-eval-after-load' to prevent eager
            ;; macro expansion from pulling (or failing to pull) in autoloaded
            ;; macros/packages.
            `(eval-after-load ',package ',(macroexp-progn body)))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))

;; Au revoir Doom...

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

(defun shan/copy-real-path ()
  "Copy the current file path to kill ring."
  (interactive)
  (kill-new buffer-file-name))

(defun shan/copy-buffer-name ()
  "Copy the current file path to kill ring."
  (interactive)
  (kill-new (buffer-name)))

(defun shan--parse-file-ref (ref)
  "Parse REF of the form \"path\", \"path:line\", \"path:line:col\" or \"path:line-end\".
Return a plist of :path, :line and :col.  Line and column are nil when absent.
The path is returned verbatim; resolving it is `shan--resolve-file-ref's job."
  (let ((ref (s-trim ref)))
    ;; Strip a trailing colon so "path:32:" parses like "path:32".
    (setq ref (replace-regexp-in-string ":\\'" "" ref))
    (if (string-match
         ;; Anchor the suffix so drive letters (C:/…) and "path:12" both work:
         ;; only trailing digit groups count as line/column.
         "\\`\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\|-[0-9]+\\)?\\'" ref)
        (list :path (match-string 1 ref)
              :line (string-to-number (match-string 2 ref))
              :col  (when (match-string 3 ref)
                      (string-to-number (match-string 3 ref))))
      (list :path ref :line nil :col nil))))

(defun shan--resolve-file-ref (path)
  "Return PATH as an absolute path if it names an existing file, else nil."
  (let ((path (ignore-errors
                (expand-file-name (substitute-in-file-name path)))))
    (when (and path (file-exists-p path) (not (file-directory-p path)))
      path)))

(defun shan--file-ref-candidates ()
  "Return the directories a relative file reference should be resolved against.
Ordered nearest-first: the current directory, the visited file's directory,
then the project and git roots."
  (delq nil
        (list default-directory
              (when buffer-file-name
                (file-name-directory buffer-file-name))
              (when (fboundp 'projectile-project-root)
                (projectile-project-root))
              (locate-dominating-file default-directory ".git"))))

(defun shan--file-ref-at-point ()
  "Return a likely file reference near point, or from the clipboard.
Checks the region, then the text around point, then the kill ring and system
clipboard, preferring the first that contains a path-like token."
  (let ((candidates
         (delq nil
               (list (when (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end)))
                     (thing-at-point 'filename t)
                     (ignore-errors (current-kill 0 t))
                     (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING))))))
    (or (cl-find-if (lambda (s)
                      (and (stringp s)
                           (string-match-p "\\`[^\n]*[/.][^\n]*\\'" (s-trim s))))
                    candidates)
        "")))

(defun shan/find-file-at-ref (ref)
  "Visit the file named by REF, jumping to any line and column it carries.
REF is a string like \"emacs/.emacs.d/activate.el:32\" as printed by grep,
compilers and code review tools.  Forms \"path:line\", \"path:line:col\" and
\"path:line-end\" are all understood, as is a bare path.

Interactively, the default is taken from the region, the symbol at point, or
the clipboard - whichever first looks like a file reference - so a pasted ref
usually just needs \\<minibuffer-local-map>\\[next-history-element] then RET."
  (interactive
   (list (read-string "Open ref: " (shan--file-ref-at-point))))
  (let* ((parsed (shan--parse-file-ref ref))
         (raw (plist-get parsed :path))
         (line (plist-get parsed :line))
         (col (plist-get parsed :col))
         (file (or (shan--resolve-file-ref raw)
                   ;; Retry the bare path against each candidate root.
                   (cl-loop for dir in (shan--file-ref-candidates)
                            for try = (shan--resolve-file-ref
                                       (expand-file-name raw dir))
                            when try return try))))
    (unless file
      (user-error "No such file: %s" raw))
    (find-file file)
    (when line
      ;; Widen first: a narrowed buffer would otherwise clamp the jump.
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (when col
        (forward-char (min (1- col)
                           (- (line-end-position) (point))))))
    file))

(defun shan/copy-file-ref ()
  "Copy the current file and line as \"path:line\", relative to the project.
The inverse of `shan/find-file-at-ref' - what you paste elsewhere."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let* ((root (or (when (fboundp 'projectile-project-root)
                     (projectile-project-root))
                   (locate-dominating-file buffer-file-name ".git")))
         (path (if root
                   (file-relative-name buffer-file-name root)
                 buffer-file-name))
         (ref (format "%s:%d" path (line-number-at-pos nil t))))
    (kill-new ref)
    (message "%s" ref)
    ref))

(global-set-key (kbd "C-c l") #'shan/find-file-at-ref)
(global-set-key (kbd "C-c L") #'shan/copy-file-ref)

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

(defun shan/symbol-append (&rest symbols)
  "Concatenate n SYMBOLS and return a symbol."
  (intern (apply #'s-concat
                 (mapcar #'symbol-name symbols))))

(defun shan/ensure-list (item)
  "Ensure that ITEM is a list. If it's not a list, wrap it in a list."
  (if (listp item)
      item
    (list item)))

(defun shan/random-element-from-list (lst)
  "Choose a random element from the given list `LST'."
  (if lst
      (nth (random (length lst)) lst)
    nil))

(defun shan/chunk-string-length-n (n str)
  "Chunk the string `STR' into groups with a maximum length of `N'."
  (if (<= n 0)
      (error "Chunk size (n) must be greater than 0")
    (let ((chunks '())
          (current-chunk "")
          (remaining-length n))
      (dolist (char (append str nil))
        (if (> remaining-length 0)
            (setq current-chunk (concat current-chunk (string char))
                  remaining-length (- remaining-length 1))
          (setq chunks (cons current-chunk chunks)
                current-chunk (string char)
                remaining-length (- n 1))))
      (setq chunks (cons current-chunk chunks))
      (nreverse chunks))))

(provide 'core-util)
;;; core-util.el ends here
