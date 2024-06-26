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

;; gcmh
(load-file "~/.emacs.d/gcmh.el")
(gcmh-mode 1)

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
;;      (append initial-frame-alist
;;	      '((left . 0)
;;		(width . 0)
;;		(fullscreen . fullboth))))

;; disable toolbar, menubar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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
      ((filename (pt/project-relative-file-name do-not-strip-prefix)))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))

(bind-key "C-c p" #'copy-file-name-to-clipboard)

;; set line numbers
(global-display-line-numbers-mode t)

(column-number-mode)
(setq-default display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; highlight current line
(global-hl-line-mode t)
(add-hook 'text-mode-hook #'hl-line-mode)

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

;; themeing
(load-file "~/.emacs.d/themes.el")

;; company-mode
(with-eval-after-load 'kaolin-themes
  (load-file "~/.emacs.d/company.el"))

;; pdf view
(pdf-loader-install)

(defun turn-off-line-numbers ()
  "It turn of line number mode when in pdf-tools mode."
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook #'turn-off-line-numbers)

;; nov.el
(defun nov-display ()
  (face-remap-add-relative 'variable-pitch :family "FiraCode Nerd Font"
						   :height 100)
  (toggle-scroll-bar -1)
  (setq mode-line-format nil
		nov-header-line-format ""
		cursor-type nil))

(setq-default visual-fill-column-center-text t)
(setq-default visual-fill-column-width 120)

;;(use-package nov
;;  :ensure t
;;  :config
;;  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
;;  (add-hook 'nov-mode-hook 'turn-off-line-numbers))

;;(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

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

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (elm-mode . lsp)
	 (go-mode . lsp)
	 (rust-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

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
  (setq emms-player-list '(emms-player-mpv)
	emms-info-functions '(emms-info-native)))

;; erc
(load-file "~/.emacs.d/erc.el")

;;go-lang
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (autoload 'go-mode "go-mode" nil t)
  (setq tab-width 4
	indent-tabs-mode 1)
  (setq lsp-go-analyses '((shadow . t)
                        (simplifycompositelit . :json-false))))

;; dart-mode
(use-package dart-mode
  :ensure t)

;; dumb-jump (jump to definition)
(use-package dumb-jump
  :ensure t)

;; electric-pair
(electric-pair-mode t)

;; electric indent
(electric-indent-mode t)

;; sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command (concat
				"/opt/local/bin/pandoc"
				" --from=markdown --to=html"
				" --standalone --mathjax --highlight-style=pygments")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(## rainbow-delimiters pdf-tools org-roam nov multiple-cursors mood-line magit lsp-mode golden-ratio god-mode erc-image emojify emms ef-themes dumb-jump dired-sidebar dart-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
