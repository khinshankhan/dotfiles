(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; turn off functionality of ctr+z
(global-unset-key (kbd "C-z"))

;; deletes all the whitespace when you hit C-c backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-set-key (kbd "C-c <backspace>") 'hungry-delete-backward)
  (global-set-key (kbd "C-c <deletechar>") 'hungry-delete-forward))

(org-babel-load-file (expand-file-name "~/.zemacs/.emacs.d/myinit.org"))
