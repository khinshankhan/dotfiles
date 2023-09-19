;;; activate.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Choose which modules to enable and how to enable them.
;;; Code:

(defvar shan--active-modules)

;;; Order slightly matters, maybe auto-ide should be part of core?
(setq shan--active-modules
      '(
        :tools
        (auto-ide +hydra-auto-ide)
        (lsp +dap +ui)
        (vc +git +gutter +hydra-git)

        :input
        macos

        :checkers
        (spell +flyspell +aspell)
        (syntax +flycheck)
        (grammar +lsp) ; requires node 16 and pnpm -g add @emacs-grammarly/grammarly-languageserver

        :completion
        (company +childframe)
        vertico
        snippets

        :editor
        editorconfig
        ;; hungry-delete ; FIXME: breaks ivy and other tools
        multiple-cursors
        (parentheses +rainbow)
        (zoom +text +window)

        :ui
        (color +todo +whitespace +nums +tokens)
        iconography               ; required for doom-modeline
        (theme +solaire)
        modeline
        dashboard                 ; TODO: customize this some more
        (hydra +hydra-window)
        discoverability

        :lang
        ;; (asm +mips)
        ;; (js +ts +jsx +tsx +vue +lsp +dap)
        ;; (go +lsp +dap) ; install via vscode for now...
        ;; (web +emmet +vtl +lsp +dap)
        ;; (graphql +lsp) ; pnpm -g add graphql graphql-language-service-cli
        ;; scala ; FIXME: wayy down the backlog, used scala for college last
        shell
        ;; (python +lsp) ; pnpm -g add pyright
        ;; swift ; FIXME: wayy down the backlog, I don't do swift dev
        (yaml +lsp) ; pnpm -g add yaml-language-server
        json

        :misc
        keyfreq
        ;; sicp
        ))

(provide 'activate)
;;; activate.el ends here
