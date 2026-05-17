;; LSP: gem install solargraph
(require 'core-straight)

(package! ruby-mode
  :mode
  (("\\.rb\\'"       . ruby-mode)
   ("\\.rake\\'"     . ruby-mode)
   ("Rakefile\\'"    . ruby-mode)
   ("Gemfile\\'"     . ruby-mode)
   ("\\.gemspec\\'"  . ruby-mode)))

(lsp! ruby-mode
  (auto-ide/add! 'ruby-mode #'hydra-lsp/body))
