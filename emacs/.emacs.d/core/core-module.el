;;; core-module.el --- define interaction for modules dir -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by Lgneous/ Doom.
;;; Code:

(require 'core-paths)
(require 'core-util)
(require 'dash)
(require 'f)

(defun core-module/add-category-to-elements (lst)
  "Add a category to each element in `LST'."
  (--map (list (car lst) (shan/ensure-list it)) (cdr lst)))

(defun core-module/categorize-modules (lst)
  "Categorize modules in `LST' based on category headers."
  (->>
   lst
   (-partition-before-pred #'keywordp)
   (-map #'core-module/add-category-to-elements)
   (apply 'append)))

(defun core-module/get-modules-in-category (categories category)
  "Get modules in a specific `CATEGORY' from `CATEGORIES' list."
  (->>
   categories
   (--filter (eq (car it) category))))

(defun core-module/get-features-in-module-in-category (categories category module)
  "Get features of a specific `MODULE' within a `CATEGORY' from `CATEGORIES' list."
  (->>
   (core-module/get-modules-in-category categories category)
   (--filter (eq (caadr it) module))
   cdadar))

(defun core-module/category-p (categories category)
  "Check if a `CATEGORY' exists in the `CATEGORIES' list.

It returns t if the category is found, and nil otherwise."
  (->>
   categories
   (--some (eq (car it) category))))

(defun core-module/module-p (categories category module)
  "Check if a `MODULE' exists within a `CATEGORY' in the `CATEGORIES' list.

It returns t if the module is found in the category, and nil otherwise."
  (->>
   (core-module/get-modules-in-category categories category)
   (--some (eq (caadr it) module))))

(defun core-module/feature-p (categories category module feature)
  "Check if a `FEATURE' exists within a `MODULE' of a `CATEGORY' in the `CATEGORIES' list.

It returns t if the feature is found in the module of the category,
and nil otherwise."
  (->>
   (core-module/get-features-in-module-in-category categories category module)
   (--some (eq it feature))))

;; Usable during and in config loading
;;; variables to make looking up current module info easy and not depend on filename or anything weird
(defvar modulation--categories)
(defvar modulation--current-category)
(defvar modulation--current-module)
(defvar modulation--current-features)

(defmacro module-p! (category module)
  "Return nil if the `MODULE' is not activated in the right `CATEGORY', t otherwise."
  (declare (indent defun))
  `(core-module/module-p modulation--categories ',category ',module))

(defmacro with-module! (category module &rest body)
  "Execute BODY if `MODULE' is activated in `CATEGORY'."
  (declare (indent defun))
  `(when (module-p! ,category ,module)
     ,@body))

(defmacro without-module! (category module &rest body)
  "Execute BODY if `MODULE' is activated in `CATEGORY'."
  (declare (indent defun))
  `(unless (module-p! ,category ,module)
     ,@body))

(defmacro feature-p! (feature)
  "Return nil if the `FEATURE' is not toggled in the current category and module, t otherwise."
  (declare (indent defun))
  `(core-module/feature-p modulation--categories modulation--current-category modulation--current-module ',feature))

(defmacro with-feature! (feature &rest body)
  "Execute `BODY' if `FEATURE' is toggled."
  (declare (indent defun))
  `(when (feature-p! ,feature)
     ,@body))

(defmacro without-feature! (feature &rest body)
  "Execute `BODY' if `FEATURE' is toggled."
  (declare (indent defun))
  `(unless (feature-p! ,feature)
     ,@body))

(defun feature-substring! (str)
  "Return all toggled features that are substrings of `'STR'."
  (->> modulation--categories
       -flatten
       (-map #'symbol-name)
       (--filter (s-contains? str it))))

;; lsp go brr
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

;; Load the config
(defun core-module/load-module (category module)
  "Load `CATEGORY'/`MODULE'/config.el in `shan-modules-dir'."
  (interactive)
  (let* ((module-name (if module
                          (f-join shan-modules-dir category module)
                        (f-no-ext
                         (f-dirname
                          (read-file-name "Enter module: " shan-modules-dir)))))
         (module-path (f-join shan-modules-dir module-name)))
    (load module-path nil 'nomessage)))

(defun core-module/load (modules)
  "Load the `MODULES'."
  (setq modulation--categories (core-module/categorize-modules modules))
  (dolist (lst modulation--categories)
    (pcase-let ((`(,category (,module . ,features)) lst))
      (setq modulation--current-category category
            modulation--current-module module
            modulation--current-features features)

      (core-module/load-module
       (-> ; :category -> "category"
        modulation--current-category
        symbol-name
        (substring 1))
       (-> ; module -> "module"
        modulation--current-module
        symbol-name)))))

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
