(require 'core-straight)

(package! nerd-icons
  :config
  ;; TODO: figure out why this unless block isn't working
  ;; (print (find-font (font-spec :name "Symbols Nerd Font Mono")))
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))
