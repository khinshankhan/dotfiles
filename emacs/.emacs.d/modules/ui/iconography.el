(require 'core-straight)

(package! nerd-icons
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline)
  :config
  (do-once-n-sec-after-emacs-startup!
   0.1
   (when (not (member "Symbols Nerd Font Mono" (font-family-list)))
     (nerd-icons-install-fonts t)
     (revert-buffer))))
