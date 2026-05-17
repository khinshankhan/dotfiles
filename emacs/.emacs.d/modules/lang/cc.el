;; LSP: clangd (usually bundled with llvm/clang)
(require 'core-straight)

(package! cc-mode
  :mode
  (("\\.c\\'"   . c-mode)
   ("\\.h\\'"   . c-mode)
   ("\\.cpp\\'" . c++-mode)
   ("\\.cc\\'"  . c++-mode)
   ("\\.hpp\\'" . c++-mode)
   ("\\.hh\\'"  . c++-mode)))

(lsp! c-mode
  (auto-ide/add! 'c-mode #'hydra-lsp/body))

(lsp! c++-mode
  (auto-ide/add! 'c++-mode #'hydra-lsp/body))
