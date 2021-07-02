(require 'pretty-hydra)
(require 'core-fboundp)

(pretty-hydra-define
  hydra-window
  (:exit nil :color pink :title " Screen" :quit-key "q")
  ("Window Split"
   (("2" split-window-below "below")
    ("3" split-window-right "right"))
   "Window Movement"
   (("b" balance-windows "balance")
    ("l" delete-window "kill" :exit t)
    ("w" other-window "move"))
   "Buffer Control"
   (("k" kill-buffer "kill" :exit t)
    ("s" shan/scratch "scratch" :exit t))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold) :exit t))))

(pretty-hydra-define+ hydra-leader ()
  ("Shortcuts"
   (("w" hydra-window/body (propertize "+screen" 'face 'bold)))))
