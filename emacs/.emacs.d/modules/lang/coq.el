;;; coq.el --- -*- lexical-binding: t; -*-
(require 'core-straight)

(package! proof-general
  :mode ("\\.v\\'" . coq-mode))
