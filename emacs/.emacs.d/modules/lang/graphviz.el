(require 'core-straight)

(package! graphviz-dot-mode
  :mode
  (("\\.dot\\'" . graphviz-dot-mode)
   ("\\.gv\\'"  . graphviz-dot-mode)))
