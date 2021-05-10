;;; modules.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Choose which modules to enable and how to enable them.

;;; Code:

(require 'core-module)

(load!
 :editor
 editorconfig
 flycheck
 ;; hungry-delete
 multiple-cursors
 (parentheses
  +rainbow)
 (yasnippets ; this module is necessary for lsp apparently (?)
  )
 zoom
 :completion
 company
 (ivy
  +swiper)
 :tools
 git
 (lsp
  +dap)
 :lang
 go
 javascript
 (web
  +restclient
  +emmet)
 :ui
 dashboard
 discoverability
 hl-todo
 modeline
 theme
 (hydra))

(provide 'modules)
;;; modules.el ends here
