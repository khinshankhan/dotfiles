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
        ;; (asm +mips)
        (go +lsp +dap)
        ;; scala                  ; FIXME: wayy down the backlog, used scala for college last
        shell
        ;; swift                  ; FIXME: wayy down the backlog, I don't do swift dev
        :ui
        dashboard                 ; TODO: customize this some more
        discoverability
        hl-todo
        (hydra +hydra-window)
        modeline
        rainbow
        (theme +solaire)
        :app
        browser
        :hobbies
        ;; medical
        :community
        keyfreq
        sicp
        wakatime
        ))

(provide 'activate)
;;; activate.el ends here
