;; init.el file

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

;; M is cmd key
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; indent
(electric-indent-mode t)
;; parens pairs
(electric-pair-mode t)
;; paren matching
(show-paren-mode t)

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

;; nswbuff
;; used for buffer switching, press C-<tab> to show a list of which buffer to switch in case
;; you have a lot of buffers to switch from
;; ----------------------------------------------------------
;; install nswbuff
(require 'nswbuff)
(global-set-key (kbd "C-<tab>") 'nswbuff-switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'nswbuff-switch-to-previous-buffer)

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
   '(## company delight golden-ratio hydra magit punch-line
	telephone-line transient with-editor)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
