(require 'core-straight)

;; Setup
;;; Ensure projectile is configured to handle JavaScript projects
(with-eval-after-load 'projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "^node_modules$"))

(package! add-node-modules-path
  :hook
  ((html-mode
    css-mode
    web-mode
    markdown-mode
    js-mode
    js2-mode
    json-mode
    rjsx-mode
    typescript-mode
    solidity-mode) . add-node-modules-path)
  :init
  (setq add-node-modules-path-command
        '(
          "pnpm bin"
          "pnpm bin -w"
          "yarn bin"
          "npm bin")))

;; Conditionally enable prettier-mode only if available
(defun +prettier-safe-to-enable-p ()
  "Return t if Prettier is installed and the current project is not explicitly excluded."
  (and (projectile-project-p)
       ;; No broken plugin file in this project
       (not (file-exists-p
             (expand-file-name "plugins/prettier-md.cjs"
                               (projectile-project-root))))
       ;; Prettier is available somewhere
       (or (executable-find "prettier")
           (let ((bin (expand-file-name
                       "node_modules/.bin/prettier"
                       (projectile-project-root))))
             (file-executable-p bin)))))

(defun +conditionally-enable-prettier ()
  "Enable prettier-mode if prettier is available and the buffer is suitable."
  (when (+prettier-safe-to-enable-p)
    (message "Prettier available -- enabling prettier-mode")
    (prettier-mode 1)))

(package! prettier
  :hook
  ((html-mode
    css-mode
    web-mode
    markdown-mode
    js-mode
    js2-mode
    json-mode
    rjsx-mode
    typescript-mode
    solidity-mode) . prettier-mode))

;; core js (js)
(package! js2-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js-chain-indent t
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1))

;; react (jsx)
(package! rjsx-mode
  :if (feature-p! +jsx)
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config
  ;; HACK `rjsx-electric-gt' relies on js2's parser to tell it when the cursor
  ;;      is in a self-closing tag, so that it can insert a matching ending tag
  ;;      at point. The parser doesn't run immediately however, so a fast typist
  ;;      can outrun it, causing tags to stay unclosed, so force it to parse:
  (defadvice! +javascript-reparse-a (n)
    ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
    :before #'rjsx-electric-gt
    (if (= n 1) (rjsx-maybe-reparse)))

  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("components/.+\\.js$" . rjsx-mode)))

;; core ts (ts)
(package! typescript-mode
  :if (feature-p! +ts)
  :hook (typescript-mode . rainbow-delimiters-mode)
  :config
  (lsp! typescript-mode
    (auto-ide/add! 'typescript-mode #'hydra-lsp/body)))

;; react (tsx)
;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
;;        `web-mode' because `typescript-mode' does not officially support
;;        JSX/TSX. See
;;        https://github.com/emacs-typescript/typescript.el/issues/4
(with-feature! +tsx
  (if (module-p! :lang web)
      (progn
        (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
        (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
        (auto-ide/add! 'typescript-tsx-mode #'hydra-lsp/body)
        ;; HACK: quick fix to get lsp in ts files, will look into later
        (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-tsx-mode))

        (with-module-feature! :lang web +emmet
          (add-hook 'typescript-tsx-mode-hook #'emmet-mode))

        (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
        (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode))

    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))))

;; TODO: look into custom snippets
(with-feature! +jsx
  (after! yasnippet
    (package! react-snippets)))
(with-feature! +tsx
  (after! yasnippet
    (package! react-snippets)))

;; vue
(with-feature! +vue
  (package! vue-html-mode)

  (package! vue-mode
    :defer t
    :mode
    (("\\.vue\\'"  . vue-mode))))

;; string interpolations
(after! graphql-mode
  (mmm-add-classes
   '((js-graphql
      :submode graphql-mode
      :face mmm-declaration-submode-face
      :front " ?\\(?:GraphQL ?\\*/ ?\\|gql\\)`" ;; match either starting with "/* GraphQl */`" or "gql`" case and space insensitive
      :back "`")))

  (dolist (mode '(js-mode js2-mode))
    (mmm-add-mode-ext-class mode nil 'js-graphql)))

(after! web-mode
  (mmm-add-classes
   '((js-html
      :submode web-mode
      :face mmm-declaration-submode-face
      :front " ?\\(?:html ?\\*/ ?\\|html\\)`" ;; match either starting with "/* GraphQl */`" or "gql`" case and space insensitive
      :back "`")))

  (dolist (mode '(js-mode js2-mode))
    (mmm-add-mode-ext-class mode nil 'js-html)))
