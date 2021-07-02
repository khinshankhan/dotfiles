(require 'core-straight)
(require 'f)
(require 'core-module)

;; Important to take note of the following chart when making hydras:
;;; |------------+-----------------------+-----------------------+-----------------|
;;; | Body Color | Head  Inherited Color | Executing NON-HEADS   | Executing HEADS |
;;; |------------+-----------------------+-----------------------+-----------------|
;;; | amaranth   | red                   | Disallow and Continue | Continue        |
;;; | teal       | blue                  | Disallow and Continue | Quit            |
;;; | pink       | red                   | Allow and Continue    | Continue        |
;;; | red        | red                   | Allow and Quit        | Continue        |
;;; | blue       | blue                  | Allow and Quit        | Quit            |
;;; |------------+-----------------------+-----------------------+-----------------|

;;; Grant 'chord' permission, basically press two keys really quickly.
(package! key-chord
  :demand t
  :config
  (setq key-chord-two-keys-delay 0.05)
  (key-chord-mode t))

;;; Add `:chords' to `use-package' and derived macros.
(package! use-package-chords
  :demand t)

(package! hydra
  :demand t
  :config
  (setq hydra--work-around-dedicated nil
        hydra-is-helpful t
        hydra-hint-display-type 'lv
        lv-use-separator nil)
  :chords
  ;; TODO: At the moment my preferred keys are `ao'. Rethinking this because it
  ;; messes up a lot words with 'oa' such as 'load'.
  ("ao" . hydra-leader/body))

(package! pretty-hydra
  :demand t)

(require 'pretty-hydra)
;;; I feel like pink hydras are the way to go, since I enjoy being about to use
;;; continuous undo or highlighting for a region based hydra command. But, feel
;;; free to change as you see fit.
;;; NOTE: These will get smartly filled based on hydras toggled on, leveraging
;;; `pretty-hydra-define+' in each 'feature'
(pretty-hydra-define
  hydra-leader
  (:exit t :color pink :title " Leader" :quit-key "q")
  ("General"
   ()
   "Short Hands"
   (("i" ibuffer "ibuffer"))
   "Shortcuts"
   ()
   "RSI Binds"
   (("u" undo "undo" :exit nil)
    ("a" (shan/exec-call-keymap 'Control-X-prefix "C-x") "C-x")
    (";" counsel-M-x "M-x")
    ("v" shan/vanilla-save "vanilla save"))))

(let ((current-dir (file-name-directory load-file-name)))
  (dolist (hydra-name (feature-substring! "+hydra"))
    (--> hydra-name
      (substring it 7)
      (f-join current-dir "hydra-maps" it)
      (load it nil 'nomessage))))
