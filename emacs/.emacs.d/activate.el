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
        (grammar
         +lsp)                    ; pnpm -g add @emacs-grammarly/grammarly-languageserver

        :completion
        company
        vertico

        :editor
        editorconfig
        ;; hungry-delete          ; FIXME: breaks ivy and other tools
        multiple-cursors
        (parentheses
         +rainbow)
        (zoom +text +window)

        :tools
        (lsp +dap +ui)
        (vc
         +git
         +hydra-git
         +forge
         +gutter)                 ; NOTE: flycheck errors take priority in fringe

        :ui
        iconography               ; required for doom-modeline
        dashboard                 ; TODO: customize this some more
        discoverability
        hl-todo
        (hydra +hydra-auto-ide +hydra-window)
        (rgb +rainbow)
        (theme +solaire)
        modeline

        :lang
        ;; (asm +mips)
        (js +ts +jsx +tsx +vue +lsp +dap)
        (go
         +lsp                     ; install via vscode for now...
         +dap)
        (web
         +emmet
         +vtl
         +lsp
         +dap)
        (graphql
         +lsp)                    ; pnpm -g add graphql graphql-language-service-cli
        (yaml
         +lsp)                    ; pnpm -g add yaml-language-server
        json
        ;; scala                  ; FIXME: wayy down the backlog, used scala for college last
        shell
        ;; swift                  ; FIXME: wayy down the backlog, I don't do swift dev

        :misc
        keyfreq
        ;; sicp
        ))

(provide 'activate)
;;; activate.el ends here
