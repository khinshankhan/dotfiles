(require 'core-straight)

(package! graphql-mode
  :mode
  (("\\.gql\\'"        . graphql-mode)
   ("\\.graphql\\'"    . graphql-mode)))

(package! graphql-doc
  :after graphql-mode)

(lsp! graphql-mode
  (shan--ide-add 'graphql-mode #'hydra-lsp/body)

  (with-eval-after-load 'lsp-graphql
    ;; redfine matching to not use gql server for non gql files
    (defun lsp-graphql-activate-p (filename &optional _)
      "Check if the GraphQL language server should be enabled based on FILENAME."
      (string-match-p (rx (one-or-more anything) "."
                          (or "graphql" "gql")eos)
                      filename))))
