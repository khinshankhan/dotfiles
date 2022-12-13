(require 'pretty-hydra)

  (pretty-hydra-define+ hydra-leader ()
    ("General"
     (("SPC" shan--ide-resolve (propertize "+ide" 'face 'bold)))))

(pretty-hydra-define hydra-lsp (:exit t :color pink :title " LSP" :quit-key "q")
  ("Find"
   (("." lsp-ui-peek-find-references "find references")
    ("d" lsp-find-definition "find definition")
    ("t" lsp-find-type-definition "find type definition"))
   "Refactor"
   (("e" lsp-rename "rename symbol at point")
    ("f" lsp-format-buffer "format buffer"))
   "Show"
   (("j" lsp-ui-imenu "symbol table")
    ("l" lsp-ui-flycheck-list "error list"))
   " Exit"
   (("DEL" hydra-leader/body (propertize "+leader" 'face 'bold)))))
