;;; web.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Ara ara, “web development” is huge... wonder if it should be split into multiple files...?
;;; Code:

(require 'core-straight)

;; The cleanest part of webdev: testing endpoints within emacs.
;; TODO: fix network
(package! restclient
  :if (feature-p! +restclient)
  :mode
  ("\\.http\\'" . restclient-mode))

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
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  :config
  ;; Highlight the element under the cursor.
  (setq-default web-mode-enable-current-element-highlight t)
  ;; built in color for most themes dont work well with my eyes
  (eval-after-load "web-mode"
    '(set-face-background 'web-mode-current-element-highlight-face "LightCoral"))

  ;; NOTE: although we're using rjsx for jsx files, tsx derives from web mode
  ;; and has a web mode content type of `jsx'
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (setq css-indent-offset 2))

(with-feature! +emmet
  (defun shan/emmet-mode-cheatsheet ()
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

(with-module! :tools lsp
  (with-module-feature! :tools lsp +dap
    (require 'dap-chrome))
  ;; TODO: maybe it isn't wise to have all of web mode hooked with lsp
  ;; but I haven't needed vanilla files in a while so we can keep it
  ;; for now
  (add-hook 'web-mode-hook #'lsp))

(with-module! :editor yasnippets
  (package! react-snippets
    :after yasnippet))

(provide 'web)
;;; web.el ends here
