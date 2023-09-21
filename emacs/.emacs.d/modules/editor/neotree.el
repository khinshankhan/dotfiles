(require 'core-straight)

;; I dont usually use gui, but this seemed fun. Used Ladicle's config https://github.com/Ladicle as inspiration
(package! neotree
  :after
  (projectile)
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :custom
  (neo-theme 'nerd2)
  (neo-window-position 'left)
  :bind
  ("C-c n c" . neotree-current-dir-toggle)
  ("C-c n p" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))

  (defun neotree-current-dir-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
             (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))
