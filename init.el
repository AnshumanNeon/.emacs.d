(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20))))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(setq initial-frame-alist (append initial-frame-alist
                                  '((left . 0)
                                    (width. . 0)
                                    (fullscreen . fullboth))))

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 15)
        ("MELPA Stable" . 0)))

(setq
 ns-function-modifier
 'hyper)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-12"))

(setq auto-revert-interval 1)            ; Refresh buffers fast
(setq create-lockfiles nil)              ; Disable lockfiles
(global-display-line-numbers-mode t)     ; set line numbers
(column-number-mode)
(setq-default display-line-numbers-type 'relative)
(global-hl-line-mode t)
(setq echo-keystrokes 0.1)               ; Show keystrokes asap
(setq enable-recursive-minibuffers t)    ; Allow recursive minibuffers
(setq frame-inhibit-implied-resize 1)    ; Don't resize frame implicitly
(setq inhibit-startup-screen t)          ; No splash screen please
(setq initial-scratch-message nil)       ; Clean scratch buffer
(setq recentf-max-saved-items 10000)     ; Show more recent files
(setq ring-bell-function 'ignore)        ; Quiet
(setq scroll-margin 1)                   ; Space between cursor and top/bottom
(setq sentence-end-double-space nil)     ; No double space
;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

(setq-default tab-width 4                       ; Smaller tabs
              fill-column 79                    ; Maximum line width
              truncate-lines t                  ; Don't fold lines
              indent-tabs-mode nil              ; Use spaces instead of tabs
              split-width-threshold 160         ; Split verticly by default
              split-height-threshold nil        ; Split verticly by default
              frame-resize-pixelwise t          ; Fine-grained frame resize
              auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere

(let ((default-directory (concat user-emacs-directory "site-lisp/")))
  (when (file-exists-p default-directory)
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
             (normal-top-level-add-subdirs-to-load-path))
           load-path))))

(fset 'yes-or-no-p 'y-or-n-p)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(put 'narrow-to-region 'disabled nil)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           scroll-bar-mode              ; No scroll bars either
           blink-cursor-mode))          ; The blinking cursor gets old
  (funcall mode 0))

(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(internal-border-width . 0))

(electric-indent-mode t)
(electric-pair-mode t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package ultra-scroll
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of using vc
  ;;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

;; A startup screen extracted from Spacemacs
(use-package dashboard
  :config
  (setq dashboard-banner-logo-title nil
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-page-separator "\n\n\n"
        dashboard-week-agenda t
        dashboard-items '((recents   . 5)
                          (agenda    . 5)
                          (projects  . 5)
                          (bookmarks . 5))
        dashboard-shortcuts '((recents   . "r")
                              (agenda    . "a")
                              (projects  . "p")
                              (bookmarks . "b")))
  (dashboard-setup-startup-hook))

(dolist (mode
         '(abbrev-mode                  ; E.g. sopl -> System.out.println
           column-number-mode           ; Show column number in mode line
           delete-selection-mode        ; Replace selected text
           dirtrack-mode                ; directory tracking in *shell*
           global-auto-revert-mode      ; Revert files when changed on disk
           global-so-long-mode          ; Mitigate performance for long lines
           recentf-mode                 ; Recently opened files
           show-paren-mode))            ; Highlight matching parentheses
  (funcall mode 1))

;; A Git porcelain inside Emacs.
(use-package magit
  :bind (:map
         custom-bindings-map
         ("C-x g" . magit-status)))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :init 
  (marginalia-mode 1))

;; dirvish
(use-package dirvish
  :after (dired)
  :init (dirvish-override-dired-mode)
  :config
  (add-hook 'dirvish-directory-view-mode-hook #'turn-off-line-numbers)
  (setq dirvish-use-mode-line 'global)

  (setq dirvish-mode-line-format
	'(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)

  (bind-key "C-c x" 'dirvish-side))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :defer t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

;; distinguish main buffer by giving other not so important buffers a darker
;; background tint
(use-package solaire-mode
  :defer t
  :config
  (solaire-mode t))

;; dim buffers that are not active (cursor not there)
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t)
  (setq dimmer-fraction 0.4)
  (setq dimmer-watch-frame-focus-events nil))

;; divide the buffers in golden ratio when splitting or opening a new buffer
(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occure-mode)))

(use-package rainbow-mode
  :hook prog-mode)

(use-package hydra
  :ensure t)

(use-package corral
  :ensure t)

(use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (bind-keys :map pdf-view-mode-map
        ("\\" . hydra-pdftools/body)
        ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
        ("g"  . pdf-view-first-page)
        ("G"  . pdf-view-last-page)
        ("l"  . image-forward-hscroll)
        ("h"  . image-backward-hscroll)
        ("j"  . pdf-view-next-page)
        ("k"  . pdf-view-previous-page)
        ("e"  . pdf-view-goto-page)
        ("u"  . pdf-view-revert-buffer)
        ("al" . pdf-annot-list-annotations)
        ("ad" . pdf-annot-delete)
        ("aa" . pdf-annot-attachment-dired)
        ("am" . pdf-annot-add-markup-annotation)
        ("at" . pdf-annot-add-text-annotation)
        ("y"  . pdf-view-kill-ring-save)
        ("i"  . pdf-misc-display-metadata)
        ("s"  . pdf-occur)
        ("b"  . pdf-view-set-slice-from-bounding-box)
        ("r"  .
         pdf-view-reset-slice)))

(use-package origami
  :ensure t)

(load-file "~/.emacs.d/hydras.el")
(load-file "~/.emacs.d/org-mode.el")
(load-file "~/.emacs.d/erc.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(corfu corral dimmer dirvish doom-modeline emojify erc-image golden-ratio
           gruvbox-theme gruvbox-themes hydra kanagawa-themes magit marginalia
           nano-modeline org-bullets org-mode org-pdfview org-super-agenda
           org-super-agenda-mode origami origami-mode pdf-tools rainbow-mode
           smart-mode-line solaire-mode ultra-scroll use-package-hydra))
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url "https://github.com/jdtsmith/ultra-scroll"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
