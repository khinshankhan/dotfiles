(require 'pretty-hydra)

  (pretty-hydra-define+ hydra-leader ()
    ("General"
     (("SPC" shan--ide-resolve (propertize "+ide" 'face 'bold)))))
