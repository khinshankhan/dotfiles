(require 'core-straight)

(package! asm-mode
  :mode "\\.as\\'"
  :bind (:map asm-mode-map
              ("<f5>" . #'compile)))

(package! mips-mode
  :if (feature-p! +mips)
  :mode "\\.mips$")
