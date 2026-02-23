;;; ----------------------------------------------
;;;                 Theme config
;;; ----------------------------------------------

;; load timu-spacegrey theme
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'timu-spacegrey-theme)
(load-theme 'timu-spacegrey :no-confirm)

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
