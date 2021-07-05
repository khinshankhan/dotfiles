;;; core-module.el --- define interaction for modules dir -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by Lgneous/ Doom.
;;; Code:

(require 'core-paths)
(require 'core-fboundp)
(require 'dash)
(require 'f)

(defvar shan--active-modules)
(defvar modulation--list nil
  "List containing the loaded modules, filled by `modulation--load'.
Contains atom or cons where the head is the module
and the rest are the features to enable for that module..")

(defvar modulation--activated nil)

;;; Variables to make looking up current module info easy
;;; and not depend on filename or anything weird.
(defvar modulation--current-category)
(defvar modulation--current-module)
(defvar modulation--current-features)

;; Utilities
(defun modulation--modules (category)
  "Return the modules given a CATEGORY by looking in the variable `modulation--list'."
  (->> modulation--list
       (--filter (eq (car it) category))
       (-map #'cdr)))

;;; Check if module is toggled
(defun modulation--module-toggled-p (category module)
  "Return nil if the MODULE is not toggled in the right CATEGORY, t otherwise."
  (->> category
       modulation--modules
       (--filter (eq (car it) module))
       car))

(defmacro module-p! (category module)
  "Return nil if the MODULE is not activated in the right CATEGORY, t otherwise."
  (declare (indent defun))
  `(not (null (modulation--module-toggled-p ',category ',module))))

(defmacro with-module! (category module &rest body)
  "Execute BODY if MODULE is activated in CATEGORY."
  (declare (indent defun))
  `(when (module-p! ,category ,module)
     ,@body))

;;; Check if feature is toggled
(defun modulation--feature-toggled-p (category module feature)
  "Return nil if the FEATURE is not toggled in the right CATEGORY and MODULE, t otherwise."
  (if (and (eq category modulation--current-category)
           (eq module modulation--current-module))
      (memq feature modulation--current-features)
    (memq feature (modulation--module-toggled-p category module))))

(defun feature-substring! (str)
  "Return all toggled features that are substrings of STR."
  (->> modulation--list
       (-map #'cddr)
       -flatten
       (-map #'symbol-name)
       (--filter (s-contains? str it))))

(defun features! ()
  "Return all toggled features under current module under current category."
  modulation--current-features)

(defmacro feature-p! (feature)
  "Return nil if the FEATURE is not toggled in the current category and module, t otherwise."
  (declare (indent defun))
  `(not (null (modulation--feature-toggled-p modulation--current-category modulation--current-module ',feature))))

(defun features-p! (features)
  "Return nil if any of the FEATURES are not toggled in the current category and module, t otherwise."
  (--all?
   (modulation--feature-toggled-p modulation--current-category modulation--current-module it)
   features))

(defmacro with-feature! (feature &rest body)
  "Execute BODY if FEATURE is toggled."
  (declare (indent defun))
  `(when (feature-p! ,feature)
     ,@body))

(defmacro with-features! (features &rest body)
  "Execute BODY if FEATURES are toggled."
  (declare (indent defun))
  `(when (features-p! ,features)
     ,@body))

;;; Check if combo of module + feature is toggled
(defmacro module-feature-p! (category module feature)
  "Return nil if FEATURE isn't toggled under MODULE under CATEGORY, t otherwise."
  (declare (indent defun))
  `(not (null (modulation--feature-toggled-p ',category ',module ',feature))))

(defmacro with-module-feature! (category module feature &rest body)
  "Execute BODY if FEATURE is toggled under MODULE under CATEGORY."
  (declare (indent defun))
  `(when (module-feature-p! ,category ,module ,feature)
     ,@body))

;;; Specific module macros which were significant enough
(defmacro lsp! (mode &rest body)
  "Add lsp to MODE if lsp feature is active for module and in general.
Will also execute any BODY code if lsp is active."
  (declare (indent defun))
  `(with-feature! +lsp
     (after! lsp-mode
       ,@body
       (add-hook (symbol-append ',mode '-hook) #'lsp))))

(defmacro dap! (&rest body)
  "Execute any BODY code if dap feature is active for module and in general."
  (declare (indent defun))
  `(with-feature! +dap
     (after! dap-mode
       ,@body)))

;; Parse modules to load them
(defun modulation--hierarchical-cons-to-pairs (predicate list default)
  "Return a list of pairs based on LIST where the left element is the last element which satisfies PREDICATE."
  (cond ((null list) nil)
        ((funcall predicate (car list))
         (modulation--hierarchical-cons-to-pairs predicate (cdr list)
                                                 (car list)))
        ((consp (car list))
         (cons `(,default . ,(car list))
               (modulation--hierarchical-cons-to-pairs predicate (cdr list) default)))
        (t (cons `(,default . (,(car list)))
                 (modulation--hierarchical-cons-to-pairs predicate (cdr list) default)))))

(defun modulation--ready-modules (modules)
  "Load the MODULES, internals of `modulation--load'."
  (modulation--hierarchical-cons-to-pairs 'keywordp modules :.))

(defun modulation--load-module (&optional category module)
  "Load CATEGORY/MODULE/config.el in `shan-modules-dir'."
  (interactive)
  (let* ((module-name (if module
                          (f-join shan-modules-dir category module)
                        (f-no-ext
                         (f-dirname
                          (read-file-name "Enter module: " shan-modules-dir)))))
         (module-path (f-join shan-modules-dir module-name)))
    (load module-path nil 'nomessage)))

(defun modulation--load-pair (pair)
  "Convert a PAIR (:category . '(module features?)) to a string \"category/module\"."
  (pcase-let ((`(,category . ,module) pair))
    (setq modulation--current-category category
          modulation--current-module (car module)
          modulation--current-features (cdr module))
    (modulation--load-module
     (-> category
         symbol-name
         (substring 1))
     (-> module
         car
         symbol-name))))

(defun modulation--load (modules)
  "Load the MODULES."
  (setq modulation--list (modulation--ready-modules modules))
  (dolist (pair modulation--list)
    (modulation--load-pair pair)
    (add-to-list 'modulation--activated pair t)))

(defun shan--load-config ()
  "Load modules based on `activate.el' in `user-emacs-directory'."
  (interactive)
  (load (expand-file-name "activate" user-emacs-directory)
        nil 'nomessage)
  (modulation--load shan--active-modules)
  (setq modulation--current-category nil
        modulation--current-module nil
        modulation--current-features nil))

(provide 'core-module)
;;; core-module.el ends here
