(require 'core-straight)

;; The builtin sh-script library works great for the usual sh, zsh, rc files,
;; but I like using it for my env files as well. These files usually follow the
;; same rules and syntax, especially for projects that can expand. I see no
;; downsides, and it's worked well so far.
(defconst sh-mode--string-interpolated-variable-regexp
  "{\\$[^}\n\\\\]*\\(?:\\\\.[^}\n\\\\]*\\)*}\\|\\${\\sw+}\\|\\$\\sw+")

(defun sh-mode--string-interpolated-variable-font-lock-find (limit)
  "Smartly fontify interpolated variables in shell, but only up to LIMIT nesting."
  (while (re-search-forward sh-mode--string-interpolated-variable-regexp limit t)
    (let ((quoted-stuff (nth 3 (syntax-ppss))))
      (when (and quoted-stuff (member quoted-stuff '(?\" ?`)))
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'font-lock-variable-name-face))))
  nil)

(font-lock-add-keywords 'sh-mode
                        `((sh-mode--string-interpolated-variable-font-lock-find))
                        'append)

(package! sh-script
  :mode
  ("\\.env\\'" . sh-mode))
