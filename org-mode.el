;;; Org-Mode config

(use-package org
  :ensure t
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  :bind (
         :map org-mode-map
         ("C-c <up>" . org-priority-up)
         ("C-c <down>" . org-priority-down))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(setq org-directory "~/org/")
(setq org-return-follows-link t)

;; keymaps
(global-set-key (kbd "C-c a") #'org-agenda)
(add-hook 'org-agenda-finalize-hook #'delete-other-windows)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;;; ----------------------------------------------
;;;                 Agenda config
;;; ----------------------------------------------

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-files '("~/org-agenda/"))

;; TODO states
(setq org-todo-keywords
      '((sequence "TODO(t!)" "PLANNING(p)" "ACITVE(a)" "HOLD(h)"  "|" "DONE(d!)" "CANCELLED(c@)" )
        ))

;; TODO colors
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "Red" :weight bold))
        ("PLANNING" . (:foreground "Blue" :weight bold))
        ("ACTIVE" . (:foreground "Cyan" :weight bold))
        ("DONE" . (:foreground "Green" :weight bold))
        ("CANCELLED" . (:foreground "Yellow" :weight bold))))

;; daily agends
(setq org-agenda-custom-commands
      '(("d" "Daily Agenda"
         ((agenda "" ((org-agenda-span 'day)
                      (org-deadline-warning-days 7)))))))

;; tags
(setq org-tag-alist
      '(
        ;; Places
        ("@home" . ?H)
        ("@school" . ?s)
        ("@coaching" . ?c)

        ;; activities
        ("@pen" . ?P)
        ("@programming" . ?p)
        ("@questions" . ?q)
        ("@lectures" . ?l)
        ("@homework" . ?h)
        ("@cinema" . ?C)))

;;; ----------------------------------------------
;;;    Aaron Beiber agenda config edition
;;; ----------------------------------------------

;; Agenda View "d"
;; (defun air-org-skip-subtree-if-priority (priority)
;;   "Skip an agenda subtree if it has a priority of PRIORITY.

;;   PRIORITY may be one of the characters ?A, ?B, or ?C."
;;   (let ((subtree-end (save-excursion (org-end-of-subtree t)))
;;         (pri-value (* 1000 (- org-lowest-priority priority)))
;;         (pri-current (org-get-priority (thing-at-point 'line t))))
;;     (if (= pri-value pri-current)
;;         subtree-end
;;       nil)))

;; (setq org-agenda-skip-deadline-if-done t)

;; (setq org-agenda-custom-commands
;;       '(
;;         ;; Daily Agenda & TODOs
;;         ("d" "Daily agenda and all TODOs"

;;          ;; Display items with priority A
;;          ((tags "PRIORITY=\"A\""
;;                 ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                  (org-agenda-overriding-header "High-priority unfinished tasks:")))

;;           ;; View 7 days in the calendar view
;;           (agenda "" ((org-agenda-span 7)))

;;           ;; Display items with priority B (really it is view all items minus A & C)
;;           (alltodo ""
;;                    ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
;;                                                    (air-org-skip-subtree-if-priority ?C)
;;                                                    (org-agenda-skip-if nil '(scheduled deadline))))
;;                     (org-agenda-overriding-header "ALL normal priority tasks:")))

;;           ;; Display items with pirority C
;;           (tags "PRIORITY=\"C\""
;;                 ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                  (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
;;           )

;;          ;; Don't compress things (change to suite your tastes)
;;          ((org-agenda-compact-blocks nil)))
;;         ))

;;; ----------------------------------------------
;;;    Org Super Agenda config edition
;;; ----------------------------------------------

(use-package org-super-agenda
  :after (org)
  :init
  (org-super-agenda-mode))

(setq org-agenda-custom-commands
      '(
        ("x" "Xuper View (Org Super Agenda)"
         (
          (agenda ""
                  (
                   (org-agenda-remove-tags t)                                       
                   (org-agenda-span 7)
                   )
                  )

          (alltodo ""
                   (
                    ;; Remove tags to make the view cleaner
                    (org-agenda-remove-tags t)
                    (org-agenda-prefix-format "  %t  %s")                    
                    (org-agenda-overriding-header "CURRENT STATUS")

                    ;; Define the super agenda groups (sorts by order)
                    (org-super-agenda-groups
                     '(
                       ;; Filter where TODO state is IN-PROGRESS
                       (:name "Currently Working"
                              :todo "ACTIVE"
                              :order 1)
                       ;; Filter where TODO state is PLANNING
                       (:name "Planning Next Steps"
                              :todo "PLANNING"
                              :order 2)
                       ;; Filter where tag is "coaching"                       
                       (:name "Coaching Work"
                              :tag "@coaching"
                              :order 3)
                       ;; Filter where tag is "school"                       
                       (:name "School work"
                              :tag "@school"
                              :order 4)
                       ;; Filter where tag is "lectures"                       
                       (:name "Lecture Backlog"
                              :tag "@lectures"
                              :order 5)
                       ;; Filter where tag is "questions"                       
                       (:name "Question Practice"
                              :tag "@questions"
                              :order 6)
                       ;; Filter where tag is "homework"
                       (:name "Homework"
                              :tag "@homework"
                              :order 7)
                       ;; Filter where tag is "pen"
                       (:name "Fountain Pen Practice"
                              :tag "@pen"
                              :order 8)
                       ;; Filter where tag is "cinema"
                       (:name "Films left to watch or currently watching"
                              :tag "@cinema"
                              :order 10)
                       ;; Filter where tag is "programming"
                       (:name "Programming"
                              :tag "@programming"
                              :order 13)
                       ;; Filter where state is TODO and the priority is A and the tag is not meeting
                       (:name "Other Important Items"
                              :and (:todo "TODO" :priority "A" :not (:tag "school") :not (:tag "coaching"))
                              :order 16)
                       ;; Filter where state is TODO and priority is B
                       (:name "General Backlog"
                              :and (:todo "TODO" :priority "B")
                              :order 19)
                       ;; Filter where the priority is C or less (supports future lower priorities)
                       (:name "Non Critical"
                              :priority<= "C"
                              :order 20)
                       )
                     )
                    )
                   )
          ))
        ))

(setq-default org-agenda-window-setup 'current-window)
