;; LSP: github.com/clojure-lsp/clojure-lsp
(require 'core-straight)

(package! clojure-mode
  :mode
  (("\\.clj\\'"  . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode)
   ("\\.cljc\\'" . clojurec-mode)
   ("\\.edn\\'"  . clojure-mode)))

(lsp! clojure-mode
  (auto-ide/add! 'clojure-mode #'hydra-lsp/body))
