;;; hydra.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'core-straight)

(package! key-chord
  :demand t
  :config
  (setq key-chord-two-keys-delay 0.05)
  (key-chord-mode t))

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
  ("ao" . hydra-leader/body))

(package! pretty-hydra
  :demand t)

;; Important to take note of the following chart when making hydras:
;;  |------------+-----------------------+-----------------------+-----------------|
;;  | Body Color | Head  Inherited Color | Executing NON-HEADS   | Executing HEADS |
;;  |------------+-----------------------+-----------------------+-----------------|
;;  | amaranth   | red                   | Disallow and Continue | Continue        |
;;  | teal       | blue                  | Disallow and Continue | Quit            |
;;  | pink       | red                   | Allow and Continue    | Continue        |
;;  | red        | red                   | Allow and Quit        | Continue        |
;;  | blue       | blue                  | Allow and Quit        | Quit            |
;;  |------------+-----------------------+-----------------------+-----------------|

;; Load up hydra maps
;; TODO: finish this up and add maps
(let ((hydra-maps (igneous--features (shan--current-category) (shan--current-module)))
      (parent-path (f-dirname load-file-name)))
  (dolist (hydra-map hydra-maps)
    (print parent-path)
    (print hydra-map)
    (print (f-join parent-path (symbol-name hydra-map)))))

(with-module! :tools lsp
  (with-module-feature! :tools lsp +dap
    (require 'dap-hydra)

  (with-eval-after-load 'dap-hydra
  ;; add start dap debug within hydra because convenience is key
  (defhydra+ dap-hydra (:exit nil :foreign-keys run)
    ("d" dap-debug "Start debug session"))

  (pretty-hydra-define+ hydra-lsp ()
    (;; these heads are added to the existing " Exit" column in hydra-lsp
     " Exit"
     (("SPC" dap-hydra "dap")))))))

(provide 'hydra)
;;; hydra.el ends here
