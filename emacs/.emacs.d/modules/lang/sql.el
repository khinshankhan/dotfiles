;; LSP: go install github.com/sqls-server/sqls@latest
(require 'core-straight)

(dolist (pattern '("\\.psqlrc\\'" "\\.sqliterc\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'sql-mode)))

(package! sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(lsp! sql-mode
  (auto-ide/add! 'sql-mode #'hydra-lsp/body))
