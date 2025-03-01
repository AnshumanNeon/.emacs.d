;;; org-mode-config --- this file defines the configuration for org-mode
;;; Commentary:
;;; Code:
(defun dw/org-mode-setup()
  "Org mode setup."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :interpreter ("org" . org-mode)
  :config
  (setq org-startup-indented 1)
  (setq org-ellipsis " ▾" org-hide-emphasis-markers t))

;; org-bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(eval-after-load 'org
  (add-hook 'org-mode #'dw/org-mode-setup))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(require 'org-indent)

(with-eval-after-load 'org
  (dolist (face '((org-level-1 . 1.7)
                  (org-level-2 . 1.6)
                  (org-level-3 . 1.5)
                  (org-level-4 . 1.4)
                  (org-level-5 . 1.3)
                  (org-level-6 . 1.2)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font "FiraCode Nerd Font Mono-12" :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-document-title nil :height 2.0)
  (set-face-attribute 'org-document-info nil :height 1.3)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

(use-package org-roam
  :after org
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (org-roam-setup))

;;; org-mode-config.el ends here
