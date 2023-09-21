(require 'core-straight)

(after! graphql-mode
  (mmm-add-classes
   '((js-graphql
      :submode graphql-mode
      :face mmm-declaration-submode-face
      :front " ?\\(?:GraphQL ?\\*/ ?\\|gql\\)`" ;; match either starting with "/* GraphQl */`" or "gql`" case and space insensitive
      :back "`")))

  (mmm-add-mode-ext-class 'js-mode nil 'js-graphql))
