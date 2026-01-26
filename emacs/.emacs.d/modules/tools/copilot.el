(require 'core-straight)

;; https://github.com/zerolfx/copilot.el
(package! copilot
  :if (feature-p! +complete)
  :demand t
  :straight (:host github :repo "copilot-emacs/copilot.el"
                   :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(package! gptel
  :if (feature-p! +chat)
  :demand t
  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-model 'claude-3.7-sonnet))
