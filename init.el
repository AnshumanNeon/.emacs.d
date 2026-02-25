;;; ----------------------------------------------
;;;                 Init config
;;; ----------------------------------------------

;; disable some useless modes
(tool-bar-mode -1)
(menu-bar-mode t)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; full-screen
(set-frame-parameter nil 'fullscreen 'fullboth)

;; set font
(set-frame-font "FiraCode Nerd Font Mono 12" nil t)

;; load themes
(load-file "~/.emacs.d/theme.el")

;; line-wrap
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(global-visual-line-mode t)

;; global line number
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(global-hl-line-mode t)
(show-paren-mode t)
(column-number-mode t)

;; replace yes/no with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; other defaults
(setq-default
 initial-scratch-message ""
 select-enable-clipboard t
 user-full-name "Anshuman Choudhary")

;; start blank
(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "new"))
(delete-other-windows)

;; import hydras
(load-file "~/.emacs.d/hydras.el")

;; import cheatsheets
(load-file "~/.emacs.d/cheatsheets.el")

;; M is cmd key
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; indent
(electric-indent-mode t)
;; parens pairs
(electric-pair-mode t)
;; paren matching
(show-paren-mode t)

;; load org config when you open an org file
;; (add-to-list 'auto-mode-alist '("\\.org" . (load-file "~/.emacs.d/org-mode.el")))
(load-file "~/.emacs.d/org-mode.el")

;; but load the org agenda right away
(load-file "~/.emacs.d/org-agenda.el")

;; company-mode (for completion)
(require 'company)
;; install company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; magit
;; install magit
(require 'magit)
(setq magit-refresh-status-buffer nil)
(global-set-key (kbd "C-c g") 'hydra-magit/body)

;; golden-ratio
;; install golden-ratio
(require 'golden-ratio)
(golden-ratio-mode t)

;; pdf-tools
;; install pdf-tools
(require 'pdf-tools)
(pdf-loader-install)
(add-to-list 'auto-mode-alist '("\\.pdf" . pdf-view-mode))
(setq-default pdf-view-use-unicode-ligther 0
	      pdf-view-display-size 'fit-page)

;; nswbuff
;; used for buffer switching, press C-<tab> to show a list of which buffer to switch in case
;; you have a lot of buffers to switch from
;; ----------------------------------------------------------
;; install nswbuff
(require 'nswbuff)
(global-set-key (kbd "C-<tab>") 'nswbuff-switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'nswbuff-switch-to-previous-buffer)

;; symbol-overlay
;; highlight all instances of a symbol and do some shit you want on it
(load-file "./.emacs.d/site-lisp/symbol-overlay/symbol-overlay.el")
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "C-c s") 'symbol-overlay-mode)
(global-set-key (kbd "C-c r") 'symbol-overlay-remove-all)
;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename

;; open cheatsheets
(cheatsheet-show)
(delete-other-windows)

;; show startup time
(defun efs/display-startup-time ()
  "Get startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; ----------------------------------------------------------
;; ----------------------------------------------------------
(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(## cheatsheet company delight golden-ratio hydra magit org-bullets
	org-super-agenda pdf-tools punch-line telephone-line transient
	with-editor)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; ----------------------------------------------
;;; ----------------------------------------------
