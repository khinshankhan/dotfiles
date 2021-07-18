(require 'core-straight)

(package! json-mode
  :mode
  ("\\.json\\'" . json-mode)
  :init
  (setq-default js-indent-level 2))
