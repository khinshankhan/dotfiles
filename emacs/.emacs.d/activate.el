;;; activate.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Choose which modules to enable and how to enable them.
;;; Code:

(defvar shan--active-modules)

;;; Order slightly matters, maybe auto-ide should be part of core?
(setq shan--active-modules
      '(
        :tools
        auto-ide
        (lsp +dap +ui)
        (vc +git +gutter)

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
        neotree

        :ui
        (color +todo +whitespace +nums +tokens)
        iconography
        (theme +solaire)
        modeline
        dashboard
        discoverability

        :lang
        ;; (asm +mips)
        (go +lsp +dap)
        (js +ts +jsx +tsx +vue +lsp +dap)
        (web +emmet +vtl +lsp +dap)
        (python +lsp +dap)
        shell
        (yaml +lsp) ; pnpm -g add yaml-language-server
        json
        (graphql +lsp)

        :misc
        (key-logger +freq +commands)
        ;; sicp
        ))

(provide 'activate)
;;; activate.el ends here
