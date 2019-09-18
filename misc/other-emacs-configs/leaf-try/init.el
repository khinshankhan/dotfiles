(defconst custom-file "/dev/zero")

(setq-default backup-inhibited t
              auto-save-default nil
              create-lockfiles nil
              make-backup-files nil)

;; -*- lexical-binding: t -*-

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(prog1 "leaf"
  (prog1 "install leaf"
    (custom-set-variables
     '(package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("melpa" . "http://melpa.org/packages/")
                          ("melpa-stable" . "http://stable.melpa.org/packages/")
                          ("melpa-stable2" . "http://melpa-stable.milkbox.net/packages/")
                          ("org" . "https://orgmode.org/elpa/"))))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf)))

  (leaf leaf
    :doc "Symplify your init.el configuration"
    :doc "Initialize leaf dependent packages"
    :config
    (leaf leaf-keywords
      :require t
      :config (leaf-keywords-init))

    (leaf hydra
      :ensure t
      :config
      (leaf *hydra-posframe
        :when (version<= "26.1" emacs-version)
        :when window-system
        :custom ((hydra-hint-display-type . 'posframe))))

    (leaf el-get
      :ensure t
      :init (unless (executable-find "git")
              (warn "'git' couldn't found. el-get can't download any packages"))
      :custom ((el-get-git-shallow-clone  . t)
               (el-get-emacswiki-base-url . "http://www.emacswiki.org/emacs/download/")))))
