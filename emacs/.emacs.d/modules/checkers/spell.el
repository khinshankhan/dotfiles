(require 'core-straight)
(require 'core-module)

(require 'ispell)

;; Don't spellcheck org blocks
(pushnew! ispell-skip-region-alist
          '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
          '("#\\+BEGIN_SRC" . "#\\+END_SRC")
          '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

;; Enable either aspell, hunspell or enchant.
;;   If no module flags are given, enable either aspell, hunspell or enchant
;;     if their binary is found.
;;   If one of the flags `+aspell', `+hunspell' or `+enchant' is given,
;;     only enable that spell checker.
(pcase (cond ((feature-p! +aspell)   'aspell)
             ((feature-p! +hunspell) 'hunspell)
             ((feature-p! +enchant)  'enchant)
             ((executable-find "aspell")    'aspell)
             ((executable-find "hunspell")  'hunspell)
             ((executable-find "enchant-2") 'enchant))
  (`aspell
   (setq ispell-program-name "aspell"
         ispell-extra-args '("--sug-mode=ultra"
                             "--run-together"))

   (unless ispell-aspell-dict-dir
     (setq ispell-aspell-dict-dir
           (ispell-get-aspell-config-value "dict-dir")))
   (unless ispell-aspell-data-dir
     (setq ispell-aspell-data-dir
           (ispell-get-aspell-config-value "data-dir")))
   (unless ispell-personal-dictionary
     (setq ispell-personal-dictionary
           (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                             shan-etc-dir)))

   (defun +spell-remove-run-together-switch-for-aspell-h ()
     (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args)))
   (add-hook 'text-mode-hook #'+spell-remove-run-together-switch-for-aspell-h)

   (defun +spell-init-ispell-extra-args-a (orig-fun &rest args)
     :around '(ispell-word flyspell-auto-correct-word)
     (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
       (ispell-kill-ispell t)
       (apply orig-fun args)
       (ispell-kill-ispell t))))

  (`hunspell
   (setq ispell-program-name "hunspell"))

  (`enchant
   (setq ispell-program-name "enchant-2"))

  (_ (error "Spell checker not found. Either install `aspell', `hunspell' or `enchant' or disable spellcheck.")))

(with-feature! +flyspell
  (package! flyspell
    :ensure nil
    :hook (((text-mode
             org-mode
             markdown
             TeX-mode
             rst-mode
             mu4e-compose
             message-mode
             git-commit) . flyspell-mode)
           ((prog-mode
             yaml-mode
             conf-mode) . flyspell-prog-mode))
    :config
    (setq flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil))

  (package! flyspell-correct
    :commands flyspell-correct-previous
    :bind
    ([remap ispell-word] . flyspell-correct-at-point)
    :config
    (if (module-p! :completion ivy)
        (package! flyspell-correct-ivy)
      (package! flyspell-correct-popup)))

  (package! flyspell-lazy
    :config
    (setq flyspell-lazy-idle-seconds 1
          flyspell-lazy-window-idle-seconds 3)
    (flyspell-lazy-mode +1)))
