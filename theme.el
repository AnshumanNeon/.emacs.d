<<<<<<< HEAD
;;; -------------------------------------
;;; Themes Config
;;; -------------------------------------

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
=======
;;; ----------------------------------------------
;;;                 Theme config
;;; ----------------------------------------------

;; load timu-spacegrey theme (old theme used was timu-spacegrey theme)
;; download the timu-spacegrey-theme.el file into site-lisp dir and uncomment
;; the following 3 lines of code if you want to use timu-spacegrey theme back:
;; (require 'timu-spacegrey-theme)
;; (load-theme 'timu-spacegrey :no-confirm)

;; use gruvbox soft theme (dark mode)
;; download gruvbox theme using M-x package-install
(load-theme 'gruvbox-dark-soft :no-confirm)

;; modeline
;; install telephone-line
;; setup telephone-line
(require 'telephone-line)
;; telephone-line config
(setq telephone-line-lhs
      '((accent . (telephone-line-vc-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (evil . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
;; enable telephone-line
(telephone-line-mode t)

;;; ----------------------------------------------
;;; ----------------------------------------------
>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
