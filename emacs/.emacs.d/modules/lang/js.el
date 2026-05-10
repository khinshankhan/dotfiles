(require 'core-straight)
(require 'core-module)

;; Setup
;;; Ensure projectile is configured to handle JavaScript projects
(with-eval-after-load 'projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "^node_modules$"))

;;; Node modules path — buffer-locally add node_modules/.bin to exec-path
(defun +js-add-node-modules-to-exec-path ()
  "Buffer-locally prepend node_modules/.bin to `exec-path' when package.json exists."
  (when-let* ((root (locate-dominating-file
                     (or buffer-file-name default-directory)
                     "package.json"))
              (bin-dir (expand-file-name "node_modules/.bin" root)))
    (when (file-directory-p bin-dir)
      (setq-local exec-path (cons bin-dir exec-path))
      (setq-local process-environment
                  (cons (concat "PATH=" bin-dir path-separator (getenv "PATH"))
                        process-environment)))))

;;; Biome detection
(defun +js-maybe-use-biome ()
  "Use biome as the formatter when biome.json or biome.jsonc exists in the project."
  (when (locate-dominating-file
         (or buffer-file-name default-directory)
         (lambda (dir)
           (or (file-exists-p (expand-file-name "biome.json" dir))
               (file-exists-p (expand-file-name "biome.jsonc" dir)))))
    (setq-local apheleia-formatter 'biome)))

;;; Apheleia mode mappings for non-standard modes
(with-module! :tools format
  (with-eval-after-load 'apheleia
    (dolist (mode '((js2-mode . prettier-javascript)
                    (rjsx-mode . prettier-javascript)
                    (typescript-tsx-mode . prettier-typescript)))
      (setf (alist-get (car mode) apheleia-mode-alist) (cdr mode)))))

(dolist (hook '(js-mode-hook js2-mode-hook json-mode-hook
               rjsx-mode-hook typescript-mode-hook
               typescript-tsx-mode-hook))
  (add-hook hook #'+js-add-node-modules-to-exec-path)
  (with-module-feature! :tools format +apheleia
    (add-hook hook #'+js-maybe-use-biome)
    (add-hook hook #'+format-enable-apheleia)
    (add-hook hook #'+format-disable-lsp-on-save)))

;; core js (js)
(package! js2-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js-chain-indent t
        js2-skip-preprocessor-directives t
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        js2-highlight-level 3
        js2-highlight-external-variables t
        js2-idle-timer-delay 0.1)
  (lsp! js2-mode))

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
    :before #'rjsx-electric-gt
    (if (= n 1) (rjsx-maybe-reparse)))

  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("components/.+\\.js$" . rjsx-mode))
  (lsp! rjsx-mode)
  (add-hook 'rjsx-mode-hook #'+js-lsp-organize-imports-on-save))

(defun +js-lsp-organize-imports ()
  "Organize imports via LSP, silently skipping if unsupported."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/codeAction"))
    (lsp-organize-imports)))

(defun +js-lsp-organize-imports-on-save ()
  "Buffer-locally add import organization to before-save-hook."
  (add-hook 'before-save-hook #'+js-lsp-organize-imports nil t))

;; core ts (ts)
(package! typescript-mode
  :if (feature-p! +ts)
  :hook ((typescript-mode . rainbow-delimiters-mode)
         (typescript-mode . +js-lsp-organize-imports-on-save))
  :config
  (lsp! typescript-mode
    (auto-ide/add! 'typescript-mode #'hydra-lsp/body)))

;; react (tsx)
(with-feature! +tsx
  (with-module! :lang web
    (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
    (lsp! typescript-tsx-mode
      (auto-ide/add! 'typescript-tsx-mode #'hydra-lsp/body))
    (add-hook 'typescript-tsx-mode-hook #'+js-lsp-organize-imports-on-save)

    (with-module-feature! :lang web +emmet
      (add-hook 'typescript-tsx-mode-hook #'emmet-mode))

    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode))

  (without-module! :lang web
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))))

;; snippets
(when (or (feature-p! +jsx) (feature-p! +tsx))
  (after! yasnippet
    (package! react-snippets)))

;; vue
(with-feature! +vue
  (package! vue-html-mode)
  (package! vue-mode
    :defer t
    :mode ("\\.vue\\'" . vue-mode)
    :hook ((vue-mode . +js-add-node-modules-to-exec-path)))

  (with-module-feature! :tools format +apheleia
    (add-hook 'vue-mode-hook #'+js-maybe-use-biome)
    (add-hook 'vue-mode-hook #'+format-enable-apheleia)
    (add-hook 'vue-mode-hook #'+format-disable-lsp-on-save))

  (with-module-feature! :lang web +emmet
    (add-hook 'vue-mode-hook #'emmet-mode)))

;; string interpolations
(after! graphql-mode
  (mmm-add-classes
   '((js-graphql
      :submode graphql-mode
      :face mmm-declaration-submode-face
      :front " ?\\(?:GraphQL ?\\*/ ?\\|gql\\)`"
      :back "`")))

  (dolist (mode '(js-mode js2-mode))
    (mmm-add-mode-ext-class mode nil 'js-graphql)))

(after! web-mode
  (mmm-add-classes
   '((js-html
      :submode web-mode
      :face mmm-declaration-submode-face
      :front " ?\\(?:html ?\\*/ ?\\|html\\)`"
      :back "`")))

  (dolist (mode '(js-mode js2-mode))
    (mmm-add-mode-ext-class mode nil 'js-html)))
