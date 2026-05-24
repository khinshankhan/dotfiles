;; LSP: go install github.com/sqls-server/sqls@latest
(require 'core-straight)

(dolist (pattern '("\\.?psqlrc\\'" "\\.?sqliterc\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'sql-mode)))

(defun shan/sql-rc-p ()
  (and buffer-file-name
       (string-match-p "\\.?\\(psqlrc\\|sqliterc\\)$" buffer-file-name)))

(package! sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(lsp! sql-mode
  (auto-ide/add! 'sql-mode #'hydra-lsp/body)

  (with-eval-after-load 'lsp-sqls
    (when-let* ((client (gethash 'sql-ls lsp-clients)))
      (ht-set (lsp--client-notification-handlers client)
              "sqlLanguageServer.finishSetup" #'ignore)))

  (defun shan/skip-lsp-for-sql-rc (&rest _)
    (not (shan/sql-rc-p)))

  (advice-add 'lsp-custom/activate-lsp :before-while #'shan/skip-lsp-for-sql-rc))
