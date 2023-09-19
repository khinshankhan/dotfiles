(require 'core-straight)
(require 'core-util)

(with-os! (darwin)
  (setq mac-command-modifier 'meta)
  ;; nice to be full screen initially since i dedicate a workspace to emacs on mac anyways
  ;; hitting the green button on mac has issues apparently
  (do-once-n-sec-after-emacs-startup!
   0.15
   (toggle-frame-maximized)))
