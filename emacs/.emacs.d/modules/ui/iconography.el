(require 'core-straight)

(package! nerd-icons
  :config
  ;; TODO: figure out why this unless block isn't working
  (unless (find-font (font-spec :name "NFM"))
    (nerd-icons-install-fonts t)))
