;;; core-module.el --- define interaction for modules dir -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired from Lgneous, but I'm more a fan of just let things load than not let them load.
;;; Code:

(require 'core-paths)
(require 's)

(defvar shan--modules nil
  "List containing the loaded modules, filled by `load!'.
Contains atom or cons where the head is the module
and the rest are the features to enable for that module..")

(defmacro load! (&rest modules)
  "Load the MODULES."
  `(progn
     (igneous--load-modules ',modules)
     (dolist (path shan--module-paths)
       (message path))))

(defun shan--load-modules (modules)
  "Filter out list of `shan-modules-path' based on MODULES."
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
  "Delete module from `shan-module-paths' depending on the if the
PAIR (:category . '(module features?)) ends with a nil."
  (pcase-let ((`(,category . ,module) pair))
    (unless (car (last module))
      (let* ((module-name (symbol-name (car module)))
             (module-file-name (concat module-name ".el"))
             (modules-to-delete '()))
        (dolist (path shan--module-paths)
          (when (s-ends-with? module-file-name path)
            (push path modules-to-delete)))
        (dolist (path modules-to-delete)
          (setq shan--module-paths (delete path shan--module-paths)))))))

(defun shan--load-module (&optional module-file-name)
  "Load MODULE-FILE-NAME."
  (interactive)
  (unless module-file-name
    (setq module-file-name
          (file-name-nondirectory (read-file-name "Enter module: " shan-modules-dir))))
  (load (expand-file-name module-file-name shan-modules-dir)))

(provide 'core-module)
;;; core-module.el ends here
