<<<<<<< HEAD
;;; ---------------------------------
;;; Init Config
;;; ---------------------------------
=======
;;; ----------------------------------------------
;;;                 Init config
;;; ----------------------------------------------
>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a

;; disable some useless modes
(tool-bar-mode -1)
(menu-bar-mode t)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; add site-lisp to load path and load stuff from it
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;; full-screen
(set-frame-parameter nil 'fullscreen 'fullboth)

;; set font
(set-frame-font "FiraCode Nerd Font Mono 12" nil t)

<<<<<<< HEAD
=======
;; load themes
(load-file "~/.emacs.d/theme.el")

>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
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
<<<<<<< HEAD
(switch-to-buffer (get-buffer-create "blank"))
=======
(switch-to-buffer (get-buffer-create "new"))
>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
(delete-other-windows)

;; save backup files to this directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; save autosave files to this directory
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))

<<<<<<< HEAD
=======
;; import hydras
(load-file "~/.emacs.d/hydras.el")

;; import cheatsheets
(load-file "~/.emacs.d/cheatsheets.el")

>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
;; M is cmd key
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; indent
(electric-indent-mode t)
;; parens pairs
(electric-pair-mode t)
;; paren matching
(show-paren-mode t)

<<<<<<< HEAD
(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; load themes and modeline
(load-file "./theme.el")

;; magit
;; install magit
(use-package magit
  :ensure t
  :init
  (setq magit-refresh-status-buffer nil))

;; symbol-overlay
;; highlight all instances of a symbol and do some shit you want on it
(use-package symbol-overlay
  :ensure t)

;; multiple cursors
;; install multiple cursors
(use-package multiple-cursors
  :ensure t)

;; load hydras
(load-file "./hydras.el")

;; golden-ratio
;; install golden-ratio
(use-package golden-ratio
  :ensure t
  :init (golden-ratio-mode t))

;; vertico
;; install vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode t)
  :config
  (setq vertico-resize t)
  (setq vertico-cycle t))

;; marginalia (presents helpful annotations for each completion)
;; install marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode t))

;; pdf-tools
;; install pdf-tools
(use-package pdf-tools
  :init
  (pdf-tools-install)
  :mode
  ("\\.pdf" . pdf-view-mode)
  :config
  (setq-default pdf-view-use-unicode-ligther 0
		pdf-view-display-size 'fit-page)
  :hook
  (pdf-view-mode-hook . (lambda () (global-display-line-numbers-mode 0))))

;; company-mode (for completion)
(use-package company
  :ensure t
  :hook
  (after-init-hook . global-company-mode))

;; load org config when you open an org file
(add-to-list 'auto-mode-alist '("\\.org" . (load-file "~/.emacs.d/org-mode.el")))

;; but load the org agenda right away
(load-file "~/.emacs.d/org-agenda.el")

;; load cheatsheets
(load-file "./cheatsheets.el")
=======
;; load org config when you open an org file
;; (add-to-list 'auto-mode-alist '("\\.org" . (load-file "~/.emacs.d/org-mode.el")))
(load-file "~/.emacs.d/org-mode.el")

;; but load the org agenda right away
(load-file "~/.emacs.d/org-agenda.el")

;; company-mode (for completion)
(require 'company)
;; install go-mode
(require 'go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook (lambda ()
			  (setq tab-width 2)
			  (setq indent-tabs-mode t)))
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

;; vertico
;; install vertico
(require 'vertico)
(vertico-mode t)
(setq vertico-resize t)
(setq vertico-cycle t)

;; marginalia (presents helpful annotations for each completion)
;; install marginalia
(require 'marginalia)
(marginalia-mode t)

;; pdf-tools
;; install pdf-tools
(require 'pdf-tools)
(pdf-loader-install)
(add-to-list 'auto-mode-alist '("\\.pdf" . pdf-view-mode))
(setq-default pdf-view-use-unicode-ligther 0
	      pdf-view-display-size 'fit-page)
(add-hook 'pdf-view-mode-hook '(global-display-line-numbers-mode 0))

;; symbol-overlay
;; highlight all instances of a symbol and do some shit you want on it
(load-file "./.emacs.d/site-lisp/symbol-overlay/symbol-overlay.el")
;; hydras already set in hydras.el

;; multiple cursors
;; install multiple cursors
(require 'multiple-cursors)
;; keybindings loaded in hydra
>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a

;; open cheatsheets
(cheatsheet-show)
(delete-other-windows)

<<<<<<< HEAD
=======
;; install haskell mode by M-x package install
(setq package-install-upgrade-built-in t)

>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
;; show startup time
(defun efs/display-startup-time ()
  "Get startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

<<<<<<< HEAD
;;; ---------------------------------
;;; ---------------------------------
=======
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
>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
<<<<<<< HEAD
   '(cheatsheet company golden-ratio gruvbox-theme hydra hydras magit
		marginalia multiple-cursors org-bullets org-habit
		org-mode org-super-agenda symbol-overlay
		telephone-line vertico)))
=======
   '(## cheatsheet company dash delight golden-ratio gruvbox-theme
	haskell-mode hydra magit magit-section marginalia
	multiple-cursors org-bullets org-super-agenda pdf-tools
	punch-line telephone-line vertico with-editor)))
>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
<<<<<<< HEAD
=======

;;; ----------------------------------------------
;;; ----------------------------------------------
>>>>>>> b69b9bd7a737bac3dae91e9adcf950a970ea4f1a
