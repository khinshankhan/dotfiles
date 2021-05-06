;;; modules.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Choose which modules to enable and how to enable them.

;;; Code:

(require 'core-module)

(load!
 :tools
 lsp
 git
 :editor
 company
 ;; editorconfig
 ;; expand-region
 flycheck
 ;; hungry-delete
 ;; multiple-cursors
 ;; parentheses
 ;; project
 zoom
 :lang
 json
 (python +cython +lsp)
 (javascript)
 :ui
 theme
 modeline
 dashboard
 (ivy +swiper))

(provide 'modules)
;;; modules.el ends here
