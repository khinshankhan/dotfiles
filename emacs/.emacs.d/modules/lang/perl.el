;; LSP: cpanm Perl::LanguageServer
(require 'core-straight)

(package! cperl-mode
  :mode
  (("\\.pl\\'"  . cperl-mode)
   ("\\.pm\\'"  . cperl-mode)
   ("\\.t\\'"   . cperl-mode)))

(lsp! cperl-mode
  (auto-ide/add! 'cperl-mode #'hydra-lsp/body))
