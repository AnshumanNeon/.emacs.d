;; hydra and my collection of hydras
;; ----------------------------------------------------------

;; install hydra
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-increase 0) "reset"))

(defhydra hydra-magit (:color blue
                              :columns 4 :quit-key "q")
  "Magit"
  ("s" magit-status "status")
  ("c" magit-clone "clone")
  ("l" magit-log-all-branches "log")
  ("b" magit-branch-popup "branch popup")
  ("r" magit-rebase-popup "rebase popup")
  ("f" magit-fetch-popup "fetch popup")
  ("P" magit-push-popup "push popup")
  ("F" magit-pull-popup "pull popup")
  ("W" magit-format-patch "format patch")
  ("$" magit-process "process"))
