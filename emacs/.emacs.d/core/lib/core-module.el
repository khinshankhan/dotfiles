;;; core-module.el --- define interaction for modules dir -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired from Lgneous, but I'm more a fan of just let things load than not let them load.
;;; Code:

(require 'core-paths)
(require 'cl-lib)
(require 'dash)
(require 'f)

(defvar shan--modules nil
  "List containing the loaded modules, filled by `load!'.
Contains atom or cons where the head is the module
and the rest are the features to enable for that module..")

(defmacro load! (&rest modules)
  "Load the MODULES."
  `(igneous--load-modules ',modules))

(defun igneous--load-modules (modules)
  "Load the MODULES, internals of `load!'."
  (dolist (pair (igneous--hierarchical-cons-to-pairs 'keywordp modules :.))
    (add-to-list 'shan--modules pair t))
  (mapcar #'igneous--load-pair shan--modules))

(defun igneous--hierarchical-cons-to-pairs (predicate list default)
  "Return a list of pairs based on LIST where the left element is the last element which satisfies PREDICATE."
  (cond ((null list) nil)
        ((funcall predicate (car list))
         (igneous--hierarchical-cons-to-pairs predicate (cdr list)
                                              (car list)))
        ((consp (car list))
         (cons `(,default . ,(car list))
               (igneous--hierarchical-cons-to-pairs predicate (cdr list) default)))
        (t (cons `(,default . (,(car list)))
                 (igneous--hierarchical-cons-to-pairs predicate (cdr list) default)))))

(defun igneous--load-pair (pair)
  "Convert a PAIR (:category . '(module features?)) to a string \"category/module\"."
  (pcase-let ((`(,category . ,module) pair))
    (-> category
      symbol-name
      (substring 1)
      (concat "/" (symbol-name (car module)))
      shan--load-module)))

(defun shan--load-module (&optional module-file-name)
  "Load MODULE-FILE-NAME."
  (interactive)
  (unless module-file-name
    (setq module-file-name
          (file-name-nondirectory (read-file-name "Enter module: " shan-modules-dir))))
  (load (expand-file-name module-file-name shan-modules-dir)))

;; utils
(defun igneous--modules (category)
  "Return the modules given a CATEGORY by looking in the variable `shan--modules'."
  (->> shan--modules
    (--filter (eq (car it) category))
    (-map #'cadr)))

(defmacro with-module! (category module &rest body)
  (declare (indent 2))
  `(when (igneous--module-activated-p ',category ',module)
     ,@body))

(defun igneous--module-activated-p (category module)
  "Return nil if the MODULE is not activated in the right CATEGORY, t otherwise."
  (memq module (igneous--modules category)))

(defmacro module-p! (category module)
  "Return nil if the MODULE is not activated in the right CATEGORY, t otherwise."
  `(igneous--module-activated-p ',category ',module))

(defmacro with-feature! (feature &rest body)
  "Execute BODY if FEATURE is activated."
  (declare (indent 1))
  `(when (igneous--feature-activated-p (igneous--current-category) (igneous--current-module) ',feature)
     ,@body))

(defmacro feature-p! (feature)
  "Return nil if the FEATURE is not activated in the current category and module, t otherwise."
  `(igneous--feature-activated-p (igneous--current-category) (igneous--current-module) ',feature))

(defun igneous--feature-activated-p (category module feature)
  "Return nil if the FEATURE is not activated in the right CATEGORY and MODULE, t otherwise."
  (memq feature (igneous--features category module)))

(defun igneous--features (category module)
  "Return the features given a CATEGORY and MODULE by looking in the variable `shan--modules'."
  (->> shan--modules
    (--filter (and (-> it car (eq category)) (-> it cadr (eq module))))
    cddar))

(defun igneous--current-category ()
  "Return the current category."
  (when (string-prefix-p shan-modules-dir load-file-name)
    (cl-flet ((add-prefix (string prefix) (concat prefix string)))
      (-> load-file-name f-split (last 2) car (add-prefix ":") intern))))

(defun igneous--current-module ()
  "Return the current module."
  (when (string-prefix-p shan-modules-dir load-file-name)
    (-> load-file-name file-name-sans-extension f-split last car intern)))

(provide 'core-module)
;;; core-module.el ends here
