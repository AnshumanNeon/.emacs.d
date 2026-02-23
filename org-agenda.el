;;; ----------------------------------------------
;;;                 Agenda config
;;; ----------------------------------------------

(global-set-key (kbd "C-c a") 'org-agenda)

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
        ("@emacs" . ?e)
        ("@questions" . ?q)
        ("@lectures" . ?l)
        ("@homework" . ?h)

        ("@cinema" . ?C)))

(with-eval-after-load 'org
  (require 'org-super-agenda)
  (org-super-agenda-mode t)
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
			 ;; Filter where tag is "emacs"
			 (:name "Emacs Stuff"
				:tag "@emacs"
				:order 11)
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
          )))

(setq-default org-agenda-window-setup 'current-window)

;; habits
(require 'org-habit)
(setq org-habit-toggle-display-in-agenda t)
(add-to-list 'org-modules 'org-habit)

;;; ----------------------------------------------
;;; ----------------------------------------------
