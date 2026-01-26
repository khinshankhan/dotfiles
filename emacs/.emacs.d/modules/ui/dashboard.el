(require 'core-straight)
(require 'core-util)

(defconst dashboard-custom--quotes '(
                                     ("Sic Parvis Magna" . "Francis Drake")
                                     ("All men dream - but not equally. Those who dream by night in the dusty recesses of their minds wake in the day to find that it was vanity. But the dreamers of the day are dangerous men, for they may act their dreams with open eyes, to make it possible. This I did." . "T.E. Lawrence")
                                     ("I did not tell half of what I saw, for I knew I would not be believed." . "Marco Polo")
                                     ("For those who prove worthy, paradise awaits; for those who prove false, behold your grim fate." . "Thomas Tew")
                                     ("I am a Man of Fortune, and I must seek my Fortune." . "Henry Avery")
                                     ("Do you ever wonder why we're always, like, wearing gloves?" . "Bobby Zimmeruski")
                                     ("Sleep is for the we(a | e)k" . "Shan")
                                     ))

(defconst dashboard-custom--max-quote-length 99)

(defun dashboard-custom/get-random-title()
  (pcase-let ((`(,quotation . ,quotee) (shan/random-element-from-list dashboard-custom--quotes)))
    (format "\"%s\"\n%s~%s"
            (string-join (shan/chunk-string-length-n dashboard-custom--max-quote-length quotation) "\n")
            (make-string
             (* (/ (min (length quotation) dashboard-custom--max-quote-length) 4) 3) ; move beginning for quotee to about 75% of previous quote line
             ?\s)
            quotee)))

(defun dashboard-custom/refresh ()
  (interactive)
  (setq dashboard-banner-logo-title (dashboard-custom/get-random-title))
  (revert-buffer))

(package! dashboard
  :demand t
  :bind
  (:map dashboard-mode-map
        ("n" . widget-forward)
        ("p" . widget-backward)
        ("g" . dashboard-custom/refresh))
  :config
  (setq dashboard-banner-logo-title (dashboard-custom/get-random-title)
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-set-init-info t
        dashboard-set-footer nil)

  ;; (setq dashboard-set-navigator t)

  (setq dashboard-items '((recents  . 10)
                          ;; (bookmarks . 5)
                          ;; (projects . 5)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          ))

  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (if shan--settings-exist?
                                     shan--preferred-logo ;; weird stuff, possibly because of no-littering
                                   'logo))

  (setq dashboard-startupify-list
        '(
          dashboard-insert-banner
          dashboard-insert-newline
          dashboard-insert-banner-title
          dashboard-insert-newline
          dashboard-insert-navigator
          dashboard-insert-newline
          dashboard-insert-init-info
          dashboard-insert-items
          dashboard-insert-newline
          ;; dashboard-insert-footer
          ))

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

  (defun dashboard-custom/set-init-info()
    (setq dashboard-init-info
          (format "%d packages loaded in %s sec."
                  (length core-straight--loaded-packages)
                  (emacs-init-time))))
  (add-hook 'after-init-hook 'dashboard-custom/set-init-info))
