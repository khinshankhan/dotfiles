(require 'core-straight)

(package! which-key
  ;; :bind
  ;; ("C-h m" . which-key-show-major-mode)
  ;; ("C-h b" . which-key-show-top-level)
  :config
  (which-key-mode t)

  (with-module! :tools lsp
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))
