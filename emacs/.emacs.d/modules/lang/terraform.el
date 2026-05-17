;; LSP: github.com/hashicorp/terraform-ls
(require 'core-straight)

(package! terraform-mode
  :mode
  (("\\.tf\\'"     . terraform-mode)
   ("\\.hcl\\'"    . terraform-mode)
   ("\\.tfvars\\'" . terraform-mode)
   ("\\.tofu\\'"   . terraform-mode)))

(lsp! terraform-mode
  (auto-ide/add! 'terraform-mode #'hydra-lsp/body))
