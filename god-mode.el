;;; package --- Summary
;;; Commentary:
;;; none
;;; Code:
(use-package god-mode
  :ensure t
  :config
  (god-mode))

(global-set-key (kbd "<escape>") #'god-local-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(defun my-god-mode-update-cursor-type ()
  "Update cursor when god-mode enabled/disabled."
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

(defun my-god-mode-update-mode-line ()
  "Update mode-line when god-mode enabled/disabled."
  (cond
   (god-local-mode
    (set-face-attribute 'mode-line nil
                        :foreground "#d7d7d2"
                        :background "#2a2a2c")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#bebed7"
                        :background "#303035"))
   (t
    (set-face-attribute 'mode-line nil
			:foreground "#bebed7"
			:background "#303035")
    (set-face-attribute 'mode-line-inactive nil
			:foreground "#404148"
			:background "#2a2a2c"))))

(add-hook 'post-command-hook #'my-god-mode-update-mode-line)
;;; god-mode.el ends here
