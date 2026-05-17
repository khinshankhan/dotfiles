;; LSP: dart language-server (bundled with Dart SDK)
(require 'core-straight)

(package! dart-mode
  :mode ("\\.dart\\'" . dart-mode))

(package! flutter
  :after dart-mode)

(lsp! dart-mode
  (auto-ide/add! 'dart-mode #'hydra-lsp/body))
