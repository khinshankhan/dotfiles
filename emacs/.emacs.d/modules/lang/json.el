(require 'core-straight)

(package! json-mode
  :mode
  (("\\.json\\'"  . json-mode)
   ("\\.jsonl\\'"       . json-mode)
   ("\\.webmanifest\\'" . json-mode))
  :init
  (setq-default js-indent-level 2))
