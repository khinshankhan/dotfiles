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
  (mapcar #'shan--load-pair shan--modules))

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

(defun shan--load-pair (pair)
  "Convert a PAIR (:category . '(module features?)) to a string \"category/module\"."
  (pcase-let ((`(,category . ,module) pair))
    (shan--load-module
     (-> category
       symbol-name
       (substring 1))
     (symbol-name (car module)))))

(defun shan--load-module (&optional category module)
  "Load CATEGORY/MODULE or CATEGORY/MODULE/MODULE."
  (interactive)
  (let* ((module-file-name (if module
                               (expand-file-name
                                (f-join category module)
                                shan-modules-dir)
                             ;; NOTE: this relies on the right module entered
                             (f-no-ext
                              (read-file-name "Enter module: " shan-modules-dir))))
         (module (f-filename module-file-name)))
    (load (if (f-exists? (concat module-file-name ".el"))
                         module-file-name
            (f-join module-file-name module))
          nil
          'nomessage)))

;; utils
(defun igneous--modules (category)
  "Return the modules given a CATEGORY by looking in the variable `shan--modules'."
  (->> shan--modules
    (--filter (eq (car it) category))
    (-map #'cadr)))

(defmacro with-module! (category module &rest body)
  "Execute BODY if MODULE is activated in CATEGORY."
  (declare (indent defun))
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
  (declare (indent defun))
  `(when (shan--feature-activated-p (shan--current-category) (shan--current-module) ',feature)
     ,@body))

(defmacro with-module-feature! (category module feature &rest body)
  "Execute BODY if FEATURE is activated in MODULE under CATEGORY."
  (declare (indent defun))
  `(when (shan--feature-activated-p ',category ',module ',feature)
     ,@body))

(defmacro feature-p! (feature)
  "Return nil if the FEATURE is not activated in the current category and module, t otherwise."
  `(shan--feature-activated-p (shan--current-category) (shan--current-module) ',feature))

(defun shan--feature-activated-p (category module feature)
  "Return nil if the FEATURE is not activated in the right CATEGORY and MODULE, t otherwise."
  (let ((feat (if (stringp feature)
                  (intern feature)
                feature)))
    (memq feat (igneous--features category module))))

(defun igneous--features (category module)
  "Return the features given a CATEGORY and MODULE by looking in the variable `shan--modules'."
  (->> shan--modules
    (--filter (and (-> it car (eq category)) (-> it cadr (eq module))))
    cddar))

(defun shan--current-category ()
  "Return the current category."
  (let ((file-name (if (string-prefix-p shan-modules-dir load-file-name)
                       load-file-name
                     (expand-file-name (buffer-name)))))
    (cl-flet ((add-prefix (string prefix) (concat prefix string)))
      (-> file-name f-split (last 2) car (add-prefix ":") intern))))

(defun shan--current-module ()
  "Return the current module."
  (let ((file-name (if (string-prefix-p shan-modules-dir load-file-name)
                       load-file-name
                     (expand-file-name (buffer-name)))))
    (-> file-name file-name-sans-extension f-split last car intern)))

(provide 'core-module)
;;; core-module.el ends here
