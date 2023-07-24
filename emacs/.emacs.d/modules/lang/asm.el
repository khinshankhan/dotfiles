(require 'core-straight)

(package! asm-mode
  :mode "\\.asm\\'"
  :defer t
  :bind (:map asm-mode-map
              ("<f5>" . #'compile)))

(package! mips-mode
  :if (feature-p! +mips)
  :defer t
  :mode "\\.mips$")
