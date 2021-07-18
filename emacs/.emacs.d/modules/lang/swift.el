(require 'core-straight)
(require 'core-module)

(package! swift-mode
  :mode
  ("\\.swift\\'" . swift-mode))

;; still figuring this out
(lsp! swift
  (with-module! :completion company
    (package! company-sourcekit
      :config
      (add-to-list 'company-backends 'company-sourcekit)))
  (package! lsp-sourcekit))

;; seems swift needs special flycheck support?
;; need to set executable path later...
;; does this require lsp?
(package! flycheck-swift
  :if (module-feature-p!! :checkers syntax +flycheck)
  :config
  (flycheck-swift-setup))
