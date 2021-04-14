(require 'core-straight)

(use-package company
  :bind
  (:map company-mode-map
        ("M-/" . company-complete))
  (:map company-active-map
        ("M-/" . company-other-backend)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :custom
  (company-require-match 'never)
  (company-dabbrev-downcase nil)
  (company-tooltip-align-annotations t)
  (company-idle-delay nil)
  (company-backends '(company-capf)))
