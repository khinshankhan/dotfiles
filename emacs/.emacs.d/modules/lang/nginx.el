(require 'core-straight)

(package! nginx-mode
  :mode
  (("nginx\\.conf\\'"  . nginx-mode)
   ("/nginx/.*\\.conf\\'" . nginx-mode)))
