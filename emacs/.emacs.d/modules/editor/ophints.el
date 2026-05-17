(require 'core-straight)

(require 'pulse)
(setq pulse-delay 0.05
      pulse-iterations 10)
(set-face-attribute 'pulse-highlight-start-face nil :background "#44475a")

(defun ophints--pulse-on-yank (&rest _)
  (pulse-momentary-highlight-region (region-beginning) (region-end)))

(defun ophints--pulse-on-undo (&rest _)
  (when (and (bound-and-true-p undo-fu-mode)
             (pulse-available-p))
    (pulse-momentary-highlight-one-line (point))))

(advice-add 'yank :after
            (lambda (&rest _)
              (pulse-momentary-highlight-region
               (mark) (point))))

(advice-add 'kill-ring-save :after
            (lambda (&rest _)
              (pulse-momentary-highlight-region
               (region-beginning) (region-end))))
