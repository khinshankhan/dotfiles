(require 'core-straight)

(package! multiple-cursors
  :config
  (global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-x r t") 'mc/edit-lines)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (define-key mc/keymap (kbd "<return>") nil))
