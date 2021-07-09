(require 'pretty-hydra)
(require 'core-fboundp)

;; Hirose Yuuji and Bob Wiener
;; https://www.emacswiki.org/emacs/WindowResize
;; TODO: pick better keys to resize with
(defun hydra-window/resize-window (&optional arg)
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	    (message
	     "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
	     arg)
	    (setq c (read-char))
	    (condition-case ()
	        (cond
	         ((= c ?h) (enlarge-window arg))
	         ((= c ?s) (shrink-window arg))
	         ((= c ?w) (enlarge-window-horizontally arg))
	         ((= c ?n) (shrink-window-horizontally arg))
	         ((= c ?\^G) (keyboard-quit))
	         ((= c ?q) (throw 'done t))
	         ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	         (t (beep)))
	      (error (beep)))))
    (message "Done.")))

;; TODO: add in window movement relative to focus window using `wind'
(pretty-hydra-define
  hydra-window
  (:exit nil :color pink :title "⛶ Screen" :quit-key "q")
  ("Split"
   (("2" split-window-below "below")
    ("3" split-window-right "right"))
   "Movement"
   (("b" balance-windows "balance")
    ("l" delete-window "kill" :exit t)
    ("w" other-window "move"))
   "Control"
   (("c" hydra-window/resize-window (propertize "resize" 'face 'bold) :exit nil))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold) :exit t)
    ("k" kill-buffer "kill" :exit t)
    ("s" shan/scratch "scratch" :exit t))))

(pretty-hydra-define+ hydra-leader ()
  ("Shortcuts"
   (("w" hydra-window/body (propertize "+screen" 'face 'bold)))))
