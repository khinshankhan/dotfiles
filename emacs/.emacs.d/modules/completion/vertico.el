(require 'core-straight)

(package! orderless
  :config
  (setq
   completion-styles '(orderless basic)
   completion-category-overrides '((file (styles basic partial-completion)))))

(package! vertico
  :straight (:host github :repo "minad/vertico"
             :files (:defaults "extensions/*")
             :includes (vertico-buffer
                        vertico-directory
                        vertico-flat
                        vertico-indexed
                        vertico-mouse
                        vertico-quick
                        vertico-repeat
                        vertico-reverse))
  :bind (:map vertico-map
              ;; More convenient directory navigation commands
              ("RET" . vertico-directory-enter))
  :init
  (vertico-mode t)
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
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
