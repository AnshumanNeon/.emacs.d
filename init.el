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
(global-hl-line-mode t)
(show-paren-mode t)
(column-number-mode t)
(setq display-line-numbers 'relative)

;; start blank
(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "new"))
(delete-other-windows)

;; cmd+z is undo
(global-set-key (kbd "C-z ") 'undo)

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
;; install company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; magit
;; instal magit


;; ----------------------------------------------------------
;; ----------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9b21c848d09ba7df8af217438797336ac99cbbbc87a08dc879e9291673a6a631"
     "fc1275617f9c8d1c8351df9667d750a8e3da2658077cfdda2ca281a2ebc914e0"
     "73911b9ca48d0db91e4016c8d6e6f6438ca9435171350343fd6b5f35a9b5ef68"
     default))
 '(package-selected-packages
   '(## company magit punch-line telephone-line transient with-editor)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
