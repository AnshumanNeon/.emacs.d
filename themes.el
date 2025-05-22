;; themes

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))

;; (use-package kanagawa-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kanagawa-wave t))

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-aurora t)
  (set-face-foreground 'line-number "#60696b")
  (set-face-foreground 'font-lock-comment-face "#60696b")
  (set-face-background 'default "#191F26")
  (setq kaolin-aurora-alt-bg 0))
