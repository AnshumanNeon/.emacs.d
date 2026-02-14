;; hydra and my collection of hydras
;; ----------------------------------------------------------

;; install hydra
(defhydra hydra-zoom (global-map "C-c =")
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-increase 0) "reset"))

;; magit
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

;; file operations
(defhydra hydra-file-operations (:color blue :columns 3 :quit-key "q")
  "File Operations"
  ("m" make-directory "make directory (mkdir)")
  ("f" delete-file "delete a file")
  ("d" delete-directory "delete a directory"))
(global-set-key (kbd "C-c f") 'hydra-file-operations/body)

;; switch windows (copied from hydra wiki)
;; -------------------------------------------
(defun my/name-of-buffers (n)
  "Return the names of the first N buffers from `buffer-list'."
  (let ((bns
         (delq nil
               (mapcar
                (lambda (b)
                  (unless (string-match "^ " (setq b (buffer-name b)))
                    b))
                (buffer-list)))))
    (subseq bns 1 (min (1+ n) (length bns)))))

;; ;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
(defun my/number-names (list)
  "Enumerate and concatenate LIST."
  (let ((i 0))
    (mapconcat
     (lambda (x)
       (format "%d. %s" (cl-incf i) x))
     list
     ", ")))

(defvar my/last-buffers nil)

(defun my/switch-to-buffer (arg)
  (interactive "p")
  (switch-to-buffer
   (nth (1- arg) my/last-buffers)))

(defun my/switch-to-buffer-other-window (arg)
  (interactive "p")
  (switch-to-buffer-other-window
   (nth (1- arg) my/last-buffers)))

(global-set-key
 "\C-o"
 (defhydra my/switch-to-buffer (:exit t
                                :body-pre (setq my/last-buffers
                                                (my/name-of-buffers 4)))
   "
_o_ther buffers: %s(my/number-names my/last-buffers)

"
   ("o" my/switch-to-buffer "this window")
   ("O" my/switch-to-buffer-other-window "other window")
   ("<escape>" nil)))

;; window operations (yes copied from hydras wiki)
;; -------------------------------------
 (defhydra hydra-window ()
   "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
_l_ →        	_Z_ reset      	_s_wap		_r_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete	
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("q" hydra-move-splitter-left)
   ("w" hydra-move-splitter-down)
   ("e" hydra-move-splitter-up)
   ("r" hydra-move-splitter-right)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("F" follow-mode)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("o" delete-other-windows)
   ("i" ace-maximize-window)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("SPC" nil)
   )
