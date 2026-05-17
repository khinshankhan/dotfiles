(require 'core-straight)

(with-module! :lang web
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode)))
