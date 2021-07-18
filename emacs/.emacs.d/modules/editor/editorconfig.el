(require 'core-straight)

(package! editorconfig
  :hook
  ((prog-mode text-mode) . editorconfig-mode)
  :config
  (editorconfig-mode 1))
