(require 'core-straight)

(package! multiple-cursors
  :bind
  (("C-S-p" . mc/mark-previous-like-this)
   ("C-S-n" . mc/mark-next-like-this)
   ("C-x r t" . mc/edit-lines)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (define-key mc/keymap (kbd "<return>") nil))
