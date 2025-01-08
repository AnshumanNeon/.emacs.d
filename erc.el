;;; package --- Summary
;;; Commentary
;;; none
;;; Code:

(setq erc-server "irc.libera.chat"
      erc-nick "anshumanneon"
      erc-user-full-name "Anshuman"
      erc-track-shorten-start 8
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

(setq erc-fill-column 120
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20)

(setq erc-track-exclude '("#emacs")
      erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
      erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
      erc-track-exclude-server-buffer t)

(use-package erc-image
  :ensure t
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image))

(use-package emojify
  :ensure t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(setq erc-track-enable-keybindings t)

;;; erc.el ends here
