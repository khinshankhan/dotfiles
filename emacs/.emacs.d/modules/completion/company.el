;;; company.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  :custom
  (company-require-match 'never)
  (company-dabbrev-downcase nil)
  (company-tooltip-align-annotations t)
  (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  ;; (company-backends '(company-capf))
  :config
  (global-company-mode))

(provide 'company)
;;; company.el ends here
