;; LSP: lsp-install-server jdtls
(require 'core-straight)

(package! lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-vmargs '("-XX:+UseParallelGC"
                           "-XX:GCTimeRatio=4"
                           "-XX:AdaptiveSizePolicyWeight=90"
                           "-Dsun.zip.disableMemoryMapping=true"
                           "-Xmx2G"
                           "-Xms100m")
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-save-actions-organize-imports t
        lsp-java-completion-favorite-static-members
        '("org.junit.Assert.*"
          "org.junit.jupiter.api.Assertions.*"
          "org.mockito.Mockito.*"
          "java.util.Objects.requireNonNull"
          "java.util.Objects.requireNonNullElse")))

(lsp! java-mode
  (dap!
    (require 'dap-java))
  (auto-ide/add! 'java-mode #'hydra-lsp/body))
