;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-temple t)
;;   (setq kaolin-temple-alt-bg t)
;;   (kaolin-treemacs-theme))

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t
  :ensure t)

;;; themes.el ends here
