;;; hungry-delete.el --- -*- lexical-binding: t; -*-
(require 'core-straight)

(package! hungry-delete
  :hook
  (prog-mode . global-hungry-delete-mode))
