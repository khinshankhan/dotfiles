(require 'core-straight)

(package! graphql-mode
  :mode
  (("\\.gql\\'"        . graphql-mode)
   ("\\.graphql\\'"    . graphql-mode)))

(package! graphql-doc
  :after graphql-mode)

(lsp! graphql-mode
  (shan--ide-add 'graphql-mode #'hydra-lsp/body))
