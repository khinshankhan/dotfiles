(require 'core-straight)

(with-module! :tools lsp
  (lsp-custom/ignore-mode 'csv-mode)
  (lsp-custom/ignore-mode 'tsv-mode))

(package! csv-mode
  :mode
  (("\\.csv\\'" . csv-mode)
   ("\\.tsv\\'" . csv-mode)))
