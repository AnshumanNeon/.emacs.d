;;; ----------------------------------------------
;;;                 Ord Mode config
;;; ----------------------------------------------

(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)
(setq org-ellipsis " ▾")
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-ellipsis "...")
;; (variable-pitch-mode 1)
;; (auto-fill-mode 0)
;; (visual-line-mode 1)
(setq evil-auto-indent nil)

(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 0)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1)))
;; (org-bullets-mode t)
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

(font-lock-add-keywords
 'org-mode
 '(("^ +\\([-*]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.5)
                (org-level-2 . 1.4)
                (org-level-3 . 1.3)
                (org-level-4 . 1.2)
                (org-level-5 . 1.1)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Helvetica" :weight 'light :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(add-hook 'org-indent-hook (lambda () (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;; ----------------------------------------------
;;; ----------------------------------------------
