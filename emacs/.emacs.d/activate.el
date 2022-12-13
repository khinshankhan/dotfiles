;;; activate.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Choose which modules to enable and how to enable them.
;;; Code:

(defvar shan--active-modules)

(setq shan--active-modules
      '(
        :checkers
        (spell +flyspell +aspell)
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

        :tools
        (lsp +dap +ui)
        (vc
         +git
         +hydra-git)

        :ui
        dashboard                 ; TODO: customize this some more
        discoverability
        hl-todo
        (hydra +hydra-auto-ide +hydra-window)
        modeline
        (rgb +rainbow)
        (theme +solaire)

        :lang
        ;; (asm +mips)
        (go +lsp +dap)
        (js +ts +jsx +tsx +vue +lsp +dap)
        json
        ;; scala                  ; FIXME: wayy down the backlog, used scala for college last
        shell
        ;; swift                  ; FIXME: wayy down the backlog, I don't do swift dev
        (web +restclient +emmet +vtl +lsp +dap)

        :app
        browser

        :hobbies
        ;; medical

        :misc
        keyfreq
        sicp
        ;; wakatime
        ))

(provide 'activate)
;;; activate.el ends here
