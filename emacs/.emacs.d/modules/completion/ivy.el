(require 'core-straight)
(require 'core-fboundp)

;; General Completion
(package! ivy
  :demand t
  :bind
  ([switch-to-buffer] . ivy-switch-buffer)
  (:map ivy-minibuffer-map
        ([remap xref-find-definitions] . shan/do-nothing)
        ([remap xref-find-definitions-other-frame] . shan/do-nothing)
        ([remap xref-find-definitions-other-window] . shan/do-nothing)
        ([remap xref-find-references] . shan/do-nothing)
        ([remap xref-find-apropos] . shan/do-nothing)
        ("<return>" . ivy-alt-done)
        ("<S-return>" . ivy-immediate-done))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-height 20
        ivy-display-style 'fancy
        ivy-format-function 'ivy-format-function-line
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-initial-inputs-alist nil)
  (ivy-mode))

;; Dialog Box
(package! counsel
  :if (feature-p! +counsel)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  ("C-x b" . counsel-switch-buffer)
  :config
  (counsel-mode t)
  ;; weird because of a top-level push in source code
  (setq-default ivy-initial-inputs-alist nil))

;;; We always use projectile so we can skip checking for it
(package! counsel-projectile
  :if (feature-p! +counsel)
  :config
  (counsel-projectile-mode t)
  (defalias 'projectile-switch-to-buffer 'counsel-projectile-switch-to-buffer)
  (defalias 'projectile-find-dir 'counsel-projectile-find-dir)
  (defalias 'projectile-find-file 'counsel-projectile-find-file)
  (defalias 'projectile-grep 'counsel-projectile-grep)
  (defalias 'projectile-switch-project 'counsel-projectile-switch-project))

;;; This depends on counsel?
(package! ivy-rich
  :if (features-p! '(+rich +counsel))
  :config
  (setq ivy-rich-parse-remote-buffer nil
        ivy-switch-buffer-faces-alist nil)

  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1)

  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
