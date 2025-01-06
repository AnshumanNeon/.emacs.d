;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-temple t)
;;   (setq kaolin-temple-alt-bg t)
;;   (kaolin-treemacs-theme))

;; (use-package ef-themes
;;   :ensure t
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (ef-themes-select 'ef-night))

;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t)
;;                (enable-theme 'ample-flat))
;;   :defer t
;;   :ensure t)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-theme.el")
;; (load-theme 'zenburn t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; themes.el ends here
