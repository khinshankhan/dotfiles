(require 'pretty-hydra)
(require 'magit)

(pretty-hydra-define
  hydra-git
  (:exit nil :color pink :title " Git" :quit-key "q")
  ("Commands"
   (("g" magit "magit" :exit t)
    ("i" magit-init "init" :exit t)
    ("c" magit-clone "clone" :exit t)
    ;; TODO: look into bringing back `git-timemachine'
    ;; ("t" git-timemachine "timemachine" :exit t)
    )
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold) :exit t))))

(pretty-hydra-define+ hydra-leader ()
  ("Short Hands"
   (("g" hydra-git/body (propertize "+git" 'face 'bold)))))
