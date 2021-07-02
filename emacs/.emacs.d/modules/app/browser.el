(require 'browse-url)

(cond
 ((executable-find "firefox")
  (setq browse-url-generic-args '("-private")
        browse-url-firefox-program "firefox"
        browse-url-generic-program "firefox"))
 ((executable-find "chromium")
  (setq browse-url-generic-args '("-incognito")
        browse-url-chromium-program "chromium"
        browse-url-generic-program "chromium"))
 ((executable-find "google-chrome")
  (setq browse-url-generic-args '("-incognito")
        browse-url-chrome-program "google-chrome"
        browse-url-generic-program "chrome")))
