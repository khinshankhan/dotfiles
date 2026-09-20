;;; gnuplot.el --- -*- lexical-binding: t; -*-
(require 'core-straight)

(package! gnuplot
  :mode ("\\.gp\\'" . gnuplot-mode))
