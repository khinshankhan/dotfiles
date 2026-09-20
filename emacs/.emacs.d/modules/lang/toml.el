;;; toml.el --- -*- lexical-binding: t; -*-
(require 'core-straight)

(package! toml-mode
  :mode ("\\.toml\\'" . toml-mode))
