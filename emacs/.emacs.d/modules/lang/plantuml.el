;;; plantuml.el --- -*- lexical-binding: t; -*-
(require 'core-straight)

(package! plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode))
