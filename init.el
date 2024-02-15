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

;; terminal bind key
(bind-key* "C-c C-;" #'execute-extended-command)

;; gcmh
(load-file "~/.emacs.d/gcmh.el")
(gcmh-mode 1)

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
(setq initial-frame-alist
      (append initial-frame-alist
	      '((left . 0)
		(width . 0)
		(fullscreen . fullboth))))

;; disable toolbar, menubar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(pixel-scroll-mode)

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
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; mood line
(load-file "~/.emacs.d/mood-line.el")

;; neotree
(with-eval-after-load 'kaolin-themes
  (with-eval-after-load 'all-the-icons
    (use-package neotree
      :ensure t)
    (keymap-global-set "C-S-T" 'neotree-toggle)))

;; god-mode
(load-file "~/.emacs.d/god-mode.el")

;; parens coloring
(use-package rainbow-delimiters
  :ensure t)

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(eval-after-load 'neotree
  (defun my/neotree-hook(_unused)
    "Make sure that line numbers are not displayed in neotree buffer."
    (setq display-line-numbers -1)
    (add-hook 'prog-mode-hook 'display-line-numbers-mode))
  (add-hook 'neo-after-create-hook 'my/neotree-hook)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; clangd and eglot
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :hook
  (c-mode-hook . eglot-ensure)
  (c++-mode-hook . eglot-ensure))

;; golden ratio to handle the buffers
(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode)))

;; org-mode
(load-file "~/.emacs.d/org-mode-config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(golden-ratio org-bullets all-the-icons treemacs god-mode ## smartparens-global-mode smartparens-mode kaolin-themes magit company-manually auto-complete aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
