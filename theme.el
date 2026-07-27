;;; ----------------------------------------------
;;;                 Theme config
;;; ----------------------------------------------

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft :no-confirm))

(use-package telephone-line
  :ensure t
  :init
  (telephone-line-mode t)
  :config
  (setq telephone-line-lhs
	'((accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
	'((nil    . (telephone-line-misc-info-segment))
          (evil . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment)))))

;;; -------------------------------------
;;; -------------------------------------
