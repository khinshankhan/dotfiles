(require 'core-straight)

;; Lifted the overlay code from Centaur Emacs (gives priority to rainbow
;; mode). The rest makes the minor mode global so it’s active all the time… but
;; global rainbow may be bad for big or messy files, so watch out!
(package! rainbow-mode
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))

  (define-globalized-minor-mode global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode 1)))
  (global-rainbow-mode 1))
