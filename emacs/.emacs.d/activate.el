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
        snippets

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
        (color +todo +whitespace +nums +tokens)
        iconography               ; required for doom-modeline
        (theme +solaire)
        modeline
        dashboard                 ; TODO: customize this some more
        (hydra +hydra-auto-ide +hydra-window)
        discoverability

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
        (python
         +lsp)                    ; pnpm -g add pyright
        ;; swift                  ; FIXME: wayy down the backlog, I don't do swift dev

        :misc
        keyfreq
        ;; sicp

        :input
        macos
        ))

(provide 'activate)
;;; activate.el ends here
