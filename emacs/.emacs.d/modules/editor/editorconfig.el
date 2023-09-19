(require 'core-straight)

(package! editorconfig
  :hook
  ((prog-mode text-mode) . editorconfig-mode)
  :config
  (editorconfig-mode 1)

  ;; Fix #5057 archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are zipped XML
  ;; formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))
