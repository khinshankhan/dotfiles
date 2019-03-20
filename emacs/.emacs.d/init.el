;;; package --- Summary
;;; Commentary:
;; (package-initialize)
;;; Code:
(org-babel-load-file (concat user-emacs-directory "myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/.orgfiles")
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (which-key ace-window avy auto-yasnippet yasnippet-snippets yasnippet expand-region ess company-jedi pip-requirements merlin tuareg cider gnuplot-mode gnuplot ggtags company-c-headers flycheck company gitignore-mode git-timemachine magit smartparens rainbow-delimiters aggressive-indent org-bullets px all-the-icons-dired all-the-icons-ivy counsel ivy dashboard doom-modeline doom-themes try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:background "#30343C"))))
 '(company-preview-common ((t (:foreground "#ABB2BF" :background "#30343C"))))
 '(company-scrollbar-bg ((t (:background "#30343C"))))
 '(company-scrollbar-fg ((t (:background "#30343C"))))
 '(company-template-field ((t (:foreground "#282C34" :background "#C678DD"))))
 '(company-tooltip ((t (:foreground "#ABB2BF" :background "#30343C"))))
 '(company-tooltip-annotation ((t (:foreground "#ABB2BF" :background "#30343C"))))
 '(company-tooltip-common ((t (:foreground "#ABB2BF" :background "#30343C"))))
 '(company-tooltip-common-selection ((t (:foreground "#ABB2BF" :background "#393F49"))))
 '(company-tooltip-mouse ((t (:background "#30343C"))))
 '(company-tooltip-selection ((t (:foreground "#ABB2BF" :background "#393F49"))))
 '(flycheck-error ((t (:underline (:style line :color "#FF5C33")))))
 '(flycheck-info ((t (:underline (:style line :color "#80FF80")))))
 '(flycheck-warning ((t (:underline (:style line :color "#FF9933"))))))
