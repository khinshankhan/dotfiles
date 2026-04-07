(require 'core-straight)

(package! lua-mode
  :mode
  (("\\.lua\\'"      . lua-mode)
   ("\\.rockspec\\'" . lua-mode))
  :init
  (setq lua-indent-level 2))
