;; install mood-line
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; customise format
;; (setq mood-line-format mood-line-format-default)
(setq mood-line-format
      (mood-line-defformat
       :left
       (((mood-line-segment-buffer-status) . " ")
        ((mood-line-segment-buffer-name)   . " : ")
        (mood-line-segment-major-mode))
       :right
       (((mood-line-segment-scroll)             . " ")
        ((mood-line-segment-cursor-position)    . "  ")
        ((when (mood-line-segment-checker) "|") . "  ")
        ((mood-line-segment-checker)            . "  "))))
