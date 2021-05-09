;;; editorconfig.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! editorconfig
  :hook
  ((prog-mode text-mode) . editorconfig-mode)
  :config
  (editorconfig-mode 1))

(provide 'editorconfig)
;;; editorconfig.el ends here
