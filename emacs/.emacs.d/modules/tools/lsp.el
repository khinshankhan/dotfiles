;;; lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! lsp-mode
  :custom
  (lsp-auto-guess-root t))

(package! dap-mode
  :if (feature-p! +dap))

(provide 'lsp)
;;; lsp.el ends here
