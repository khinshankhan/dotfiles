(require 'core-straight)

(use-package orderless
  :config
  (setq
   completion-styles '(orderless basic)
   completion-category-overrides '((file (styles basic partial-completion)))))

(package! vertico
  :bind (:map vertico-map (("RET" . vertico-exit)))
  :init
  (vertico-mode t)
  :config
  (setq vertico-resize t
        vertico-cycle t
        vertico-count 20))

(package! consult
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap projectile-grep] . consult-grep)
  ("C-s" . consult-line)
  ("C-S-s" . consult-focus-lines)
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

(package! marginalia
  :init
  (marginalia-mode))

(setq enable-recursive-minibuffers t)
