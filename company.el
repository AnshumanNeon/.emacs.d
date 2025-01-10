;;; package -- Summary
;;; Commentary:

;;; Code:
(use-package company-manually
  :defer t
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.0))

(global-company-mode t)

(provide 'company)
;;; company.el ends here
