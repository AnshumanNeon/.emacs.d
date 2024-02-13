;;; package --- Summary
;;; Commentary:
;;; Code:

;; Disable startup screen
(setq inhibit-startup-message t)

;; set size of startup screen
(setq initial-frame-alist
      (append initial-frame-alist
	      '((left . 0)
		(width . 0)
		(fullscreen . fullboth))))

;; disable toolbar, menubar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; set line numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; add melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Uncomment and comment the previous one to have melpa stable
;; most users won't need this
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; checking for use-package and installing if not done yet
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; themeing
(setq theme-file "~/.emacs.d/themes.el")
(load-file theme-file)

;; company-mode
(setq company-file "~/.emacs.d/company.el")
(load-file company-file)

;; aggressive indentation
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(global-aggressive-indent-mode 1)

;; git integration
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; flychecker
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; smart parentheses
(use-package smartparens-mode
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

;; mood line
(setq mood-line-file "~/.emacs.d/mood-line.el")
(load-file mood-line-file)

;; emojis
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(add-hook 'after-init-hook #'global-emojify-mode)

;; neotree
(require 'all-the-icons)

(use-package neotree
  :ensure t)
(require 'neotree)
(keymap-global-set "C-S-T" 'neotree-toggle)

;; god-mode
(setq god-mode-file "~/.emacs.d/god-mode.el")
(load-file god-mode-file)

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :if (displ ay-graphic-p))

(defun my/neotree-hook(_unused)
  (setq display-line-numbers -1)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))
(add-hook 'neo-after-create-hook 'my/neotree-hook)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons treemacs god-mode rainbow-delimiters emojify ## smartparens-global-mode smartparens-mode kaolin-themes spacemacs-theme magit graphene company-manually auto-complete aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
