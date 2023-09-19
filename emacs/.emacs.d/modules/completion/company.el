(require 'core-straight)

(package! company
  :bind
  (:map company-mode-map
        ("C-/" . company-complete))
  (:map company-active-map
        ("C-/" . company-other-backend)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 15
        company-idle-delay nil
        company-require-match 'never
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t
        company-auto-complete nil
        company-auto-commit nil
        company-auto-complete-chars nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        ;; TODO: figure out backends nonsense someday
        company-backends '(company-capf))
  (global-company-mode))

(package! company-box
  :if (feature-p! +childframe)
  :hook (company-mode . company-box-mode))

(with-module! :tools lsp
  (add-hook 'lsp-mode-hook #'company-mode))
