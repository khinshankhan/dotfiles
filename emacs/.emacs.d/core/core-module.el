;;; core-module.el --- define interaction for modules dir -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by Lgneous/ Doom.
;;
;; Module state is stored on category symbol plists for O(1) lookup:
;;   (get :lang 'js) => '(+ts +jsx +tsx +vue +lsp +dap) or nil
;;   (get :lang 'markdown) => t (module active, no features)
;;   (get :lang 'svelte) => nil (module not active)
;;
;; Loading is two-pass:
;;   1. Parse the full module list and register all plists
;;   2. Load module files in declaration order
;; This ensures modules can reference each other regardless of
;; declaration order. Without this, js.el's (with-module! :lang web ...)
;; would silently fail because web.el hasn't loaded yet to register
;; its plist -- breaking typescript-tsx-mode which derives from web-mode.
;;
;; During loading, `modulation--current-category', `modulation--current-module',
;; and `modulation--current-features' are set so macros like `feature-p!' and
;; `with-feature!' resolve against the module being loaded.
;;; Code:

(require 'core-paths)
(require 'core-util)
(require 'dash)
(require 'f)

;;; Lookups -- O(1) via symbol plists

(defun core-module/module-p (category module)
  "Check if `MODULE' is active under `CATEGORY'."
  (not (null (get category module))))

(defun core-module/feature-p (category module feature)
  "Check if `FEATURE' is active for `MODULE' under `CATEGORY'."
  (let ((features (get category module)))
    (and (listp features) (memq feature features))))

;;; Current module context -- set during loading

(defvar modulation--current-category nil)
(defvar modulation--current-module nil)
(defvar modulation--current-features nil)

;;; Macros -- same interface as before

(defmacro module-p! (category module)
  "Return non-nil if `MODULE' is activated in `CATEGORY'."
  (declare (indent defun))
  `(core-module/module-p ',category ',module))

(defmacro with-module! (category module &rest body)
  "Execute `BODY' if `MODULE' is activated in `CATEGORY'."
  (declare (indent defun))
  `(when (module-p! ,category ,module)
     ,@body))

(defmacro without-module! (category module &rest body)
  "Execute `BODY' if `MODULE' is not activated in `CATEGORY'."
  (declare (indent defun))
  `(unless (module-p! ,category ,module)
     ,@body))

(defmacro feature-p! (feature)
  "Return non-nil if `FEATURE' is toggled for the current module."
  (declare (indent defun))
  `(core-module/feature-p modulation--current-category modulation--current-module ',feature))

(defmacro with-feature! (feature &rest body)
  "Execute `BODY' if `FEATURE' is toggled."
  (declare (indent defun))
  `(when (feature-p! ,feature)
     ,@body))

(defmacro without-feature! (feature &rest body)
  "Execute `BODY' if `FEATURE' is not toggled."
  (declare (indent defun))
  `(unless (feature-p! ,feature)
     ,@body))

(defmacro with-module-feature! (category module feature &rest body)
  "Execute `BODY' if `FEATURE' is toggled under `MODULE' under `CATEGORY'."
  (declare (indent defun))
  `(when (core-module/feature-p ,category ',module ',feature)
     ,@body))

;;; Convenience macros for common patterns

(defmacro lsp! (mode &rest body)
  "Add lsp to MODE if lsp feature is active for module and in general.
Will also execute any BODY code if lsp is active."
  (declare (indent defun))
  `(with-feature! +lsp
     (after! lsp-mode
       ,@body
       (add-hook (shan/symbol-append ',mode '-hook) #'lsp-custom/activate-lsp 'append))))

(defmacro dap! (&rest body)
  "Execute any BODY code if dap feature is active for module and in general."
  (declare (indent defun))
  `(with-feature! +dap
     (after! dap-mode
       ,@body)))

(defmacro auto-ide/add! (mode hydra)
  "Add MODE and HYDRA as (mode . hydra) to `auto-ide--alist'."
  (declare (indent defun))
  `(with-module! :tools auto-ide
     (auto-ide/add ,mode ,hydra)))

;;; Loading

(defun core-module/load-module (category module)
  "Load `CATEGORY'/`MODULE' config file from `shan-modules-dir'."
  (interactive)
  (let* ((module-name (if module
                          (f-join shan-modules-dir category module)
                        (f-no-ext
                         (f-dirname
                          (read-file-name "Enter module: " shan-modules-dir)))))
         (module-path (f-join shan-modules-dir module-name)))
    (load module-path nil 'nomessage)))

(defun core-module/load (modules)
  "Parse `MODULES' list and load each module.
Stores module state on category symbol plists for O(1) lookup.
All plists are populated first so modules can reference each other."
  (let ((category nil)
        (to-load '()))
    ;; first pass: register all modules on plists
    (dolist (entry modules)
      (cond
       ((keywordp entry)
        (setq category entry))
       (t
        (let* ((entry (if (listp entry) entry (list entry)))
               (module (car entry))
               (features (cdr entry)))
          (put category module (or features t))
          (push (list category module features) to-load)))))
    ;; second pass: load module files in order
    (dolist (item (nreverse to-load))
      (let ((category (nth 0 item))
            (module (nth 1 item))
            (features (nth 2 item)))
        (setq modulation--current-category category
              modulation--current-module module
              modulation--current-features features)
        (core-module/load-module
         (substring (symbol-name category) 1)
         (symbol-name module))))))

(defun core-module/load-config ()
  "Load modules based on `activate.el' in `user-emacs-directory'."
  (interactive)
  (load (expand-file-name "activate" user-emacs-directory)
        nil 'nomessage)
  (core-module/load shan--active-modules)
  (setq modulation--current-category nil
        modulation--current-module nil
        modulation--current-features nil))

(provide 'core-module)
;;; core-module.el ends here
