;; Hydras

                                        ; zoom

(defhydra hydra-zoom (global-map "C-c")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

                                        ; profiler

(defun profiler-running-modes ()
  (let ((running-modes
         (-non-nil (list (if (profiler-cpu-running-p) "cpu")
                         (if (profiler-memory-running-p) "mem")))))
    (if running-modes
        (s-join "+" running-modes)
      "stopped")))

(defhydra hydra-profiler (global-map "C-c p") (:columns 2 :exit t)
  "
elisp profiling (currently %s(profiler-running-modes))

^^Start / stop                          Reporting
^-^----------------------------------   ^-^----------------------------
_s_: start (prompt for mode)            _r_: show report
_c_: start CPU profiling
_m_: start memory profiling             _f_: find profile
_b_: start both CPU+memory profiling    _4_: find profile other window
_._: stop profiling                     _5_: find profile other frame
_R_: reset profiler logs

_q_: quit
_C_: customize profiler options
"
  ("s" profiler-start)
  ("c" (profiler-start 'cpu))
  ("m" (profiler-start 'mem))
  ("b" (profiler-start 'cpu+mem))
  ("." profiler-stop)
  ("R" profiler-reset)
  ("q" nil)
  ("C" (customize-group "profiler"))
  ("r" profiler-report :color blue)
  ("f" profiler-find-profile)
  ("4" profiler-find-profile-other-window)
  ("5" profiler-find-profile-other-frame))

                                        ; corral

(defhydra hydra-corral (:columns 4)
  "Corral"
  ("(" corral-parentheses-backward "Back")
  (")" corral-parentheses-forward "Forward")
  ("[" corral-brackets-backward "Back")
  ("]" corral-brackets-forward "Forward")
  ("{" corral-braces-backward "Back")
  ("}" corral-braces-forward "Forward")
  ("." hydra-repeat "Repeat")
  ("q" nil "quit" :color blue))

(global-set-key (kbd "C-c c") #'hydra-corral/body)

                                        ; pdf-tools

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))

                                      ; origami code folding

(defhydra hydra-folding (:color red)
  "
  _o_pen node    _n_ext fold       toggle _f_orward  _s_how current only
  _c_lose node   _p_revious fold   toggle _a_ll      _q_uit
  "
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("f" origami-forward-toggle-node)
  ("a" origami-toggle-all-nodes)
  ("s" origami-show-only-node)
  ("q" nil))

(global-set-key (kbd "H-C-f") 'hydra-folding/body)
