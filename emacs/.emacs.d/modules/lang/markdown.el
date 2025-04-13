(require 'core-straight)
(require 'core-module)

(use-package markdown-mode
  :init
  (setq markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-gfm-additional-languages '("sh")
        markdown-make-gfm-checkboxes-buttons t
        markdown-fontify-whole-heading-line t
        markdown-fontify-code-blocks-natively t
        ;; LaTeX support, currently failing
        markdown-enable-math nil

        ;; https://github.com/russross/blackfriday
        markdown-command "blackfriday")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mdx\\'" . markdown-mode))
  :config
  ;; HACK: Prevent mis-fontification of YAML metadata blocks in `markdown-mode'
  ;;   which occurs when the first line contains a colon in it. See
  ;;   jrblevin/markdown-mode#328.
  (defadvice! +markdown-disable-front-matter-fontification-a (&rest _)
    :override #'markdown-match-generic-metadata
    (ignore (goto-char (point-max))))

  ;; jsx and tsx are currently breaking
  (dolist (pair '(("js" . typescript-mode)
                  ("ts" . typescript-mode)
                  ("jsx" . typescript-mode)
                  ("tsx" . typescript-mode)))
    (add-to-list 'markdown-code-lang-modes pair)))
