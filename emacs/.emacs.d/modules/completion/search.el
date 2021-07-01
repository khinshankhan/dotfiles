(require 'core-straight)

(package! swiper
  :bind
  ("C-s" . swiper-isearch)
  ("C-r" . swiper-isearch-backward)
  :config
  (add-to-list 'swiper-font-lock-exclude #'dashboard-mode))
