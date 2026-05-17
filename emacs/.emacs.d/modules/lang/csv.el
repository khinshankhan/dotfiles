(require 'core-straight)

(package! csv-mode
  :mode
  (("\\.csv\\'" . csv-mode)
   ("\\.tsv\\'" . csv-mode)))
