;; LSP: opam install ocaml-lsp-server
(require 'core-straight)

(package! tuareg
  :mode
  (("\\.ml\\'"  . tuareg-mode)
   ("\\.mli\\'" . tuareg-mode)))

(package! dune
  :mode
  (("\\(?:dune\\|dune-project\\|dune-workspace\\)\\'" . dune-mode)))

(lsp! tuareg-mode
  (auto-ide/add! 'tuareg-mode #'hydra-lsp/body))
