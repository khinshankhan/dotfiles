(require 'core-module)

(with-module! :lang web
  (define-derived-mode astro-mode web-mode "Astro")
  (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-mode))

  ;; Node tooling
  (with-module! :lang js
    (add-hook 'astro-mode-hook #'+js-add-node-modules-to-exec-path))

  (with-module-feature! :tools format +apheleia
    (add-hook 'astro-mode-hook #'+js-maybe-use-biome)
    (add-hook 'astro-mode-hook #'+format-enable-apheleia)
    (add-hook 'astro-mode-hook #'+format-disable-lsp-on-save))

  ;; Emmet
  (with-module-feature! :lang web +emmet
    (add-hook 'astro-mode-hook #'emmet-mode))

  ;; LSP — lsp-mode's built-in lsp-astro client uses @astrojs/language-server
  ;; lsp-astro.el has two bugs:
  ;;   1. lsp-dependency uses :path "astroserver" but the npm binary is "astro-ls"
  ;;   2. lsp-stdio-connection uses a bare '("astro-ls" "--stdio") which
  ;;      executable-find can't resolve (it's in lsp's npm dir, not on PATH)
  ;; Fix: override both the dependency path and re-register the client to resolve
  ;; the binary via lsp-package-path instead of a bare command.
  (lsp! astro-mode
    (add-to-list 'lsp-language-id-configuration '(astro-mode . "astro"))
    (with-eval-after-load 'lsp-astro
      (lsp-dependency 'astro-language-server
                      '(:system "astro-ls")
                      '(:npm :package "@astrojs/language-server"
                             :path "astro-ls"))
      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-stdio-connection
                         (lambda ()
                           `(,(lsp-package-path 'astro-language-server) "--stdio")))
        :activation-fn (lsp-activate-on "astro")
        :initialization-options #'lsp-astro--get-initialization-options
        :server-id 'astro-ls
        :download-server-fn (lambda (_client callback error-callback _update?)
                              (lsp-package-ensure 'astro-language-server
                                                  callback error-callback)))))))
