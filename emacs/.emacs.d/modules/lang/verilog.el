(require 'core-straight)

(package! verilog-mode
  :mode
  (("\\.sv\\'"  . verilog-mode)
   ("\\.svh\\'" . verilog-mode)))

(package! vhdl-mode
  :mode
  (("\\.vhd\\'"  . vhdl-mode)
   ("\\.vhdl\\'" . vhdl-mode)))
