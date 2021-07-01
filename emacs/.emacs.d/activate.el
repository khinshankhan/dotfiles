;;; activate.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Choose which modules to enable and how to enable them.
;;; Code:

(defvar shan--active-modules)

(setq shan--active-modules
      '(
        :tools
        (auto-ide +hydra-auto-ide)
        (lsp +dap +ui)
        (vc
         +git
         +hydra-git)
        :appearance
        dashboard                 ; TODO: customize this some more
        discoverability
        hl-todo
        (hydra +hydra-window)
        modeline
        rainbow
        (theme +solaire)
        :checkers
        (syntax +flycheck)
        :completion
        company
        (ivy +counsel +rich)
        search
        (yasnippets +snippets)
        :editor
        editorconfig
        ;; hungry-delete          ; FIXME: breaks ivy and other tools
        ;; multiple-cursors       ; FIXME: false negative activations
        (parentheses
         +rainbow)
        (zoom +text +window)
        :lang
        (go +lsp +dap)
        :hobbies
        ;; medical
        :community
        browser
        keyfreq
        sicp
        wakatime
        ))

(provide 'activate)
;;; activate.el ends here
