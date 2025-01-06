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
                        :foreground "#689d6a"
                        :background "#282828")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#689d6a"
                        :background "#282828"))
   (t
    (set-face-attribute 'mode-line nil
			:foreground "#a89984"
			:background "#282828")
    (set-face-attribute 'mode-line-inactive nil
			:foreground "#a89984"
			:background "#282828"))))

(add-hook 'post-command-hook #'my-god-mode-update-mode-line)
;;; god-mode.el ends here
