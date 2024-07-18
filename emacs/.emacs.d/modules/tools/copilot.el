(require 'core-straight)

;; https://github.com/zerolfx/copilot.el
(package! copilot
  :demand t
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))
