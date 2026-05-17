(dolist (pattern '("\\.config\\'" "\\.ini\\'" "\\.properties\\'" "\\.rasi\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'conf-mode)))

(add-to-list 'auto-mode-alist '("\\.list\\'" . conf-unix-mode))
