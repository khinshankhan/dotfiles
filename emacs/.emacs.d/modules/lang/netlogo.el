(defun netlogo--grey-out-gui-section ()
  "Dim everything after the first @#$#@#$#@ separator."
  (remove-overlays (point-min) (point-max) 'netlogo-gui t)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^@#\\$#@#\\$#@$" nil t)
      (let ((ov (make-overlay (match-beginning 0) (point-max))))
        (overlay-put ov 'netlogo-gui t)
        (overlay-put ov 'face '(:foreground "#6272a4"))))))

(defvar netlogo-mode-syntax-table
  (let ((st (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?? "w" st)
    st))

(defconst netlogo-keywords
  '("to" "to-report" "end" "let" "set" "ifelse" "if" "else"
    "ifelse-value" "while" "repeat" "ask" "ask-concurrent"
    "report" "foreach" "map" "stop" "die" "run" "runresult"
    "carefully" "error" "every" "wait" "loop" "startup"
    "with" "of" "and" "or" "not" "xor"
    "in-radius" "in-cone" "at-points"
    "with-min" "with-max" "min-one-of" "max-one-of"
    "min-n-of" "max-n-of" "up-to-n-of" "who-are-not"
    "with-local-randomness" "without-interruption"
    "sort-on"))

(defconst netlogo-builtins
  '("breed" "globals" "patches-own" "turtles-own" "links-own"
    "extensions" "__includes" "directed-link-breed" "undirected-link-breed"))

(defconst netlogo-commands
  '("clear-all" "ca" "clear-turtles" "ct" "clear-patches" "cp"
    "clear-links" "clear-output" "clear-drawing" "cd" "clear-globals"
    "clear-ticks" "clear-all-plots" "clear-plot"
    "reset-ticks" "reset-timer" "reset-perspective" "rp"
    "tick" "tick-advance" "setup-plots" "update-plots"
    "create-turtles" "crt" "create-ordered-turtles" "cro"
    "create-links-with" "create-links-to" "create-links-from"
    "create-link-with" "create-link-to" "create-link-from"
    "hatch" "sprout"
    "forward" "fd" "back" "bk" "left" "lt" "right" "rt"
    "move-to" "face" "facexy" "setxy" "home" "jump"
    "pen-up" "pu" "pen-down" "pd" "pen-erase" "pe"
    "stamp" "stamp-erase"
    "hide-turtle" "ht" "show-turtle" "st"
    "hide-link" "show-link"
    "tie" "untie"
    "follow" "follow-me" "ride" "ride-me" "watch" "watch-me"
    "diffuse" "diffuse4" "downhill" "downhill4" "uphill" "uphill4"
    "import-pcolors" "import-pcolors-rgb" "import-drawing" "import-world"
    "export-world" "export-view" "export-interface" "export-output"
    "export-plot" "export-all-plots"
    "resize-world" "set-patch-size" "set-default-shape"
    "file-open" "file-close" "file-close-all" "file-delete" "file-flush"
    "file-print" "file-write" "file-type" "file-show"
    "show" "print" "type" "write" "beep"
    "output-print" "output-show" "output-type" "output-write"
    "plot" "plotxy" "histogram"
    "set-plot-pen-color" "set-plot-pen-mode" "set-plot-pen-interval"
    "set-current-plot" "set-current-plot-pen"
    "set-histogram-num-bars" "set-plot-background-color"
    "set-plot-x-range" "set-plot-y-range"
    "plot-pen-down" "plot-pen-up" "plot-pen-reset"
    "create-temporary-plot-pen"
    "auto-plot-off" "auto-plot-on"
    "auto-plot-x-off" "auto-plot-x-on"
    "auto-plot-y-off" "auto-plot-y-on"
    "display" "no-display"
    "inspect" "stop-inspecting" "stop-inspecting-dead-agents"
    "random-seed" "new-seed"
    "set-current-directory"
    "layout-circle" "layout-radial" "layout-spring" "layout-tutte"
    "hubnet-broadcast" "hubnet-broadcast-clear-output"
    "hubnet-broadcast-message" "hubnet-clear-override"
    "hubnet-clear-overrides" "hubnet-fetch-message"
    "hubnet-kick-client" "hubnet-kick-all-clients"
    "hubnet-reset" "hubnet-reset-perspective"
    "hubnet-send" "hubnet-send-clear-output" "hubnet-send-follow"
    "hubnet-send-message" "hubnet-send-override" "hubnet-send-watch"))

(defconst netlogo-reporters
  '("color" "xcor" "ycor" "heading" "size" "shape" "label" "label-color"
    "hidden?" "pen-size" "pen-mode" "who" "breed"
    "pcolor" "plabel" "plabel-color" "pxcor" "pycor"
    "end1" "end2" "thickness" "tie-mode" "link-heading" "link-length"
    "ticks" "self" "myself" "other" "nobody" "subject"
    "patch-here" "patch-ahead" "patch-at" "patch-at-heading-and-distance"
    "patch-left-and-ahead" "patch-right-and-ahead"
    "neighbors" "neighbors4"
    "turtles" "patches" "links"
    "turtle" "patch" "link"
    "turtle-set" "patch-set" "link-set"
    "no-turtles" "no-patches" "no-links"
    "turtles-at" "turtles-here" "turtles-on"
    "link-neighbors" "link-neighbor?" "link-with"
    "in-link-neighbor?" "in-link-neighbors" "in-link-from"
    "out-link-neighbor?" "out-link-neighbors" "out-link-to"
    "both-ends" "other-end"
    "my-links" "my-in-links" "my-out-links"
    "random" "random-float" "random-xcor" "random-ycor"
    "random-pxcor" "random-pycor"
    "random-exponential" "random-gamma" "random-normal" "random-poisson"
    "count" "any?" "all?" "one-of" "n-of"
    "sort" "sort-by" "shuffle"
    "max" "min" "mean" "median" "modes" "sum"
    "variance" "standard-deviation"
    "abs" "int" "remainder" "mod" "precision" "round" "ceiling" "floor"
    "ln" "log" "exp" "sqrt" "sin" "cos" "tan" "asin" "acos" "atan"
    "distance" "distancexy" "dx" "dy"
    "towards" "towardsxy" "subtract-headings" "can-move?"
    "date-and-time" "timer"
    "word" "list" "length" "item" "first" "last"
    "but-first" "butfirst" "bf" "but-last" "butlast" "bl"
    "empty?" "member?" "position" "reverse"
    "remove" "remove-item" "replace-item" "insert-item"
    "fput" "lput" "sentence" "sublist" "substring"
    "n-values" "reduce" "filter"
    "is-turtle?" "is-patch?" "is-link?" "is-number?" "is-string?"
    "is-list?" "is-boolean?" "is-agent?" "is-agentset?"
    "is-anonymous-command?" "is-anonymous-reporter?"
    "is-directed-link?" "is-undirected-link?"
    "is-link-set?" "is-patch-set?" "is-turtle-set?" "is-breed?"
    "mouse-xcor" "mouse-ycor" "mouse-down?" "mouse-inside?"
    "world-width" "world-height"
    "max-pxcor" "max-pycor" "min-pxcor" "min-pycor"
    "patch-size" "netlogo-version" "netlogo-web?"
    "shapes" "base-colors" "approximate-hsb" "approximate-rgb"
    "extract-hsb" "extract-rgb" "hsb" "rgb" "scale-color"
    "shade-of?" "wrap-color"
    "error-message" "read-from-string"
    "user-directory" "user-file" "user-new-file"
    "user-input" "user-message" "user-one-of" "user-yes-or-no?"
    "file-at-end?" "file-exists?" "file-read" "file-read-characters"
    "file-read-line" "home-directory"
    "plot-name" "plot-pen-exists?" "autoplot?" "autoplotx?" "autoploty?"
    "plot-x-min" "plot-x-max" "plot-y-min" "plot-y-max"
    "behaviorspace-experiment-name" "behaviorspace-run-number"
    "hubnet-clients-list" "hubnet-enter-message?" "hubnet-exit-message?"
    "hubnet-message" "hubnet-message-source" "hubnet-message-tag"
    "hubnet-message-waiting?"))

(define-derived-mode netlogo-mode prog-mode "NetLogo"
  :syntax-table netlogo-mode-syntax-table
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (highlight-numbers-mode -1)
  (font-lock-add-keywords
   nil
   `((,(regexp-opt netlogo-keywords 'symbols) . font-lock-keyword-face)
     (,(regexp-opt netlogo-builtins 'symbols) . font-lock-builtin-face)
     (,(regexp-opt netlogo-commands 'symbols) . font-lock-function-name-face)
     ("\\b\\(true\\|false\\|nobody\\|e\\|pi\\|black\\|white\\|gray\\|grey\\|red\\|orange\\|brown\\|yellow\\|green\\|lime\\|turquoise\\|cyan\\|sky\\|blue\\|violet\\|magenta\\|pink\\)\\b" . font-lock-constant-face)
     (,(regexp-opt netlogo-reporters 'symbols) . font-lock-variable-name-face)
     ("\\bto\\(?:-report\\)?\\s-+\\(\\w+\\)" 1 font-lock-function-name-face)
     ("\\bask\\s-+\\(\\w+\\)" 1 font-lock-type-face)))
  (add-hook 'after-change-functions (lambda (&rest _) (netlogo--grey-out-gui-section)) nil t)
  (netlogo--grey-out-gui-section))

(add-to-list 'auto-mode-alist '("\\.nlogo\\'" . netlogo-mode))
(add-to-list 'auto-mode-alist '("\\.nls\\'" . netlogo-mode))
