(require 'core-straight)

(package! default-text-scale
  :if (feature-p! +text)
  :init
  (default-text-scale-mode)
  :config
  ;; HACK: try preemptively setting the font to my preferred font size
  (do-once-n-sec-after-emacs-startup!
   0.1
   (default-text-scale-increment
     ;; TODO: account for different machines properly
     (- (if (equal system-type 'darwin)
            150
          120)
        (face-attribute 'default :height)))))

(package! zoom-window
  :if (feature-p! +window)
  :bind
  ("C-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "#130321"))
