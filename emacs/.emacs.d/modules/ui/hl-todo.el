;;; hl-todo.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Helpful to go through documentation in comments from other devs (or message
;; to myself!).  Stole the sane keywords and colors from Doom.  It seems to
;; break shell mode special operators though…
;;; Code:

(require 'core-straight)

(package! hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(provide 'hl-todo)
;;; hl-todo.el ends here
