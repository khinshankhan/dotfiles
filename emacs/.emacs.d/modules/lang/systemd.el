(require 'core-straight)

(package! systemd
  :mode
  (("\\.service\\'"   . systemd-mode)
   ("\\.timer\\'"     . systemd-mode)
   ("\\.target\\'"    . systemd-mode)
   ("\\.mount\\'"     . systemd-mode)
   ("\\.automount\\'" . systemd-mode)
   ("\\.slice\\'"     . systemd-mode)
   ("\\.socket\\'"    . systemd-mode)
   ("\\.path\\'"      . systemd-mode)
   ("\\.netdev\\'"    . systemd-mode)
   ("\\.network\\'"   . systemd-mode)
   ("\\.link\\'"      . systemd-mode)))
