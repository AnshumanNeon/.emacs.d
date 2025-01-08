;;; package --- Summary
;;; Commentary:
;;; Code:

;; Disable startup screen
(setq inhibit-startup-message t)

;; default settings
(setq initial-scratch-message nil)
(setq mark-even-if-inactive nil)
(setq kill-whole-line t)
(setq use-short-answers t)
(setq completions-detailed t)
(setq next-error-message-highlight t)
(delete-selection-mode t)
(savehist-mode)

(defun server-shutdown()
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(global-set-key (kbd "C-x #") 'server-shutdown)

(add-hook 'emacs-startup-hook (lambda ()
                                (when (get-buffer "*scratch*")
				  (delete-other-windows))))

;; font
(add-to-list 'default-frame-alist
	     '(font . "FiraCode Nerd Font Mono-10"))

;; get startup times
(defun efs/display-startup-time ()
  "Get startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; set size of startup screen
;; (setq initial-frame-alist
;;       (append initial-frame-alist
;; 	      '((left . 0)
;; 		(width . 0)
;; 		(fullscreen . fullboth))))

(setq frame-resize-pixelwise t)
(toggle-frame-fullscreen)

;; disable toolbar, menubar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(pixel-scroll-mode t)
(pixel-scroll-precision-mode t)

;; highlight links
(global-goto-address-mode)

;; copy filename to clipboard
(defun copy-file-name-to-clipboard (do-not-strip-prefix)
  "Copy the current buffer file name to the clipboard using DO-NOT-STRIP-PREFIX."
  (interactive "P")
  (let
      ((filename (file-relative-name do-not-strip-prefix)))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(bind-key "C-c p" #'copy-file-name-to-clipboard)

;; set line numbers
(global-display-line-numbers-mode t)

(column-number-mode)
(setq-default display-line-numbers-type 'relative)

;; highlight current line
(global-hl-line-mode t)

;; remove backup and autosave files
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

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

;; gcmh
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode t))

;; themeing
(load-file "~/.emacs.d/themes.el")

;; company-mode
(with-eval-after-load 'doom-themes
  (load-file "~/.emacs.d/company.el"))

;; pdf view
(use-package pdf-tools
  :ensure t
  :config
  (pdf-loader-install))

(defun turn-off-line-numbers ()
  "It turn of line number mode when in pdf-tools mode."
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook #'turn-off-line-numbers)

;; (use-package nov
;;  :ensure t
;;  :config
;;  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
;;  (add-hook 'nov-mode-hook 'turn-off-line-numbers))

;; nov.el
;; (defun nov-display ()
;;   (face-remap-add-relative 'variable-pitch :family "FiraCode Nerd Font"
;; 						   :height 100)
;;   (toggle-scroll-bar -1)
;;   (setq mode-line-format nil
;; 		nov-header-line-format ""
;; 		cursor-type nil))

;; (setq-default visual-fill-column-center-text t)
;; (setq-default visual-fill-column-width 120)

;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;(setq nov-text-width 80)
;;(setq nov-text-width t)
;;(setq nov-text-width 120)

;; aggressive indentation bad so I use electric indent mode
(electric-indent-mode 1)

;; git integration
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; flychecker
(flymake-mode t)

;; mood line
(load-file "~/.emacs.d/mood-line.el")

;; god-mode
(load-file "~/.emacs.d/god-mode.el")

;; parens coloring
(use-package rainbow-delimiters
  :ensure t)

;; golden ratio to handle the buffers
(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode)))

;; org-mode
(load-file "~/.emacs.d/org-mode-config.el")

;;go-lang
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (autoload 'go-mode "go-mode" nil t)
  (setq tab-width 4
	indent-tabs-mode 1))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (rust-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)))

;; music-player emms
(use-package emms
  :ensure t
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mplayer)))

;; erc
(load-file "~/.emacs.d/erc.el")

;; dimmer.el
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t)
  (setq dimmer-fraction 0.4)
  (setq dimmer-watch-frame-focus-events nil))

;; focus-mode
(use-package focus
  :ensure t
  :config
  (global-set-key (kbd "C-c C-#") 'focus-mode))


;; ;; dart-mode
;; (use-package dart-mode
;;   :ensure t)

;; ;; flutter-mode
;; (use-package flutter
;;   :after dart-mode
;;   :config
;;   (flutter-sdk-path "~/development/flutter"))

;; dumb-jump (jump to definition)
(use-package dumb-jump
  :ensure t)

;; electric-pair
(electric-pair-mode t)

;; electric indent
(electric-indent-mode t)

;; dirvish
(use-package dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode))

;; discord
(load-file "./elcord.el")
(require 'elcord)
(elcord-mode)

;; remember the last place in a file
(save-place-mode t)

;; eaf apps
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-rss-reader)
;; (require 'eaf-image-viewer)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-org-previewer)

;; use-package with package.el:
(use-package dashboard
  :ensure t
  :custom (initial-buffer-choice 'dashboard-open))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gcmh focus dimmer org-autolist doom-themes zenburn-theme gruvbox-theme emojify erc-image erc-hl-nicks ef-themes emacsql-sqlite org-roam dired-sidebar elcord ample-theme dumb-jump flutter dart-mode go-mode emms lsp-mode nov visual-fill-column golden-ratio org-bullets all-the-icons treemacs god-mode ## smartparens-global-mode smartparens-mode kaolin-themes magit company-manually auto-complete aggressive-indent))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
