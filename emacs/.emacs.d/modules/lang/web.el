(require 'core-straight)
(require 'core-module)

;; TODO: learn these someday, I think I've only use `!' and `C-j' for
;; expansions, but emmet has so much more potential
;; TODO: look into emmet for jsx/ tsx?
(with-feature! +emmet
  (defun emmet-custom/cheatsheet ()
    "Open emmet mode cheatsheet"
    (interactive)
    (browse-url "https://docs.emmet.io/cheatsheet-a5.pdf"))

  (package! emmet-mode
    :hook
    ((html-mode
      css-mode
      web-mode
      markdown-mode
      js-mode
      js2-mode
      json-mode
      rjsx-mode
      typescript-mode
      typescript-tsx-mode
      solidity-mode
      php-mode
      sgml-mode) . emmet-mode)))

;; TODO: I have to seriously go through http://web-mode.org/ some day
(package! web-mode
  :mode
  (("\\.html?\\'"       . web-mode)
   ("\\.phtml\\'"       . web-mode)
   ("\\.tpl\\.php\\'"   . web-mode)
   ("\\.blade\\.php\\'" . web-mode)
   ("\\.php$"           . my/php-setup)
   ("\\.[agj]sp\\'"     . web-mode)
   ("\\.as[cp]x\\'"     . web-mode)
   ("\\.erb\\'"         . web-mode)
   ;; ("\\.jsx\\'"         . web-mode)
   ;; ("\\.tsx\\'"         . web-mode)
   ("\\.mustache\\'"    . web-mode)
   ("\\.djhtml\\'"      . web-mode))
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1
        web-mode-attr-indent-offset 2
        web-mode-block-padding 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-markup-indent-offset 2)

  ;; Highlight the element under the cursor.
  (setq-default web-mode-enable-current-element-highlight t)
  ;; NOTE: built in color for most themes dont work well with my eyes
  ;; LightCoral fits nicely with doom-dracula
  (eval-after-load "web-mode"
    '(set-face-background 'web-mode-current-element-highlight-face "LightCoral"))

  ;; Use // instead of /* as the default comment delimited in JS
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal)
        "//")

  ;; NOTE: although we're using rjsx for jsx files, tsx derives from web mode
  ;; and has a web mode content type of `jsx'
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (setq css-indent-offset 2)

  ;; TODO: embedded elixir files, will need to refine when I get to writing
  ;; actual elixir
  (with-module! :lang elixir
    (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))))

;; this is better than vtl-mode package?
(with-feature! +vtl
  (define-derived-mode vtl-mode web-mode "vtl")
  (add-to-list 'lsp-custom--ignore-alist 'vtl-mode)
  (add-to-list 'auto-mode-alist '("\\.vtl$" . vtl-mode)))

;; TODO: maybe it isn't wise to have all of web mode hooked with lsp
;; but I haven't needed vanilla files in a while so we can keep it
;; for now
(lsp! web-mode
  (dap!
    (require 'dap-chrome)))

;; TODO: set company backends
(with-module! :completion company
  (package! company-web))
