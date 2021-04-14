;;; modules.el -*- lexical-binding: t; -*-

(require 'core-module)

(load!
 :tools
 lsp
 :editor
 company
 ;; editorconfig
 ;; expand-region
 ;; flycheck
 ;; hungry-delete
 ;; multiple-cursors
 ;; parentheses
 ;; project
 zoom
 :lang
 (python +cython +lsp)
 :ui
 theme
 modeline
 dashboard)

(provide 'modules)
;;; modules.el ends here
