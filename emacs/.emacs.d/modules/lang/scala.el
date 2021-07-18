(require 'core-straight)
(require 'core-module)

(package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(package! sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; TODO: scala metals refuses to cooperate
;; https://scalameta.org/metals/docs/editors/emacs.html
(lsp! scala
  (package! lsp-metals))
