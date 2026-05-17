(require 'core-straight)

(lsp-custom/ignore-mode 'gitattributes-mode)

(package! git-modes
  :mode
  (("\\.gitconfig\\'"     . gitconfig-mode)
   ("\\.dockerignore\\'"  . gitignore-mode)
   ("\\.eslintignore\\'"  . gitignore-mode)
   ("\\.prettierignore\\'" . gitignore-mode)
   ("\\.helmignore\\'"    . gitignore-mode)))
