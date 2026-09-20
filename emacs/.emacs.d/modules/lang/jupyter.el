;;; jupyter.el --- -*- lexical-binding: t; -*-
(require 'core-straight)

(package! ein
  :mode ("\\.ipynb\\'" . ein:ipynb-mode))
