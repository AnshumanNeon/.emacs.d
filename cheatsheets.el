;; cheatsheets.el file

;; install cheatsheets
(require 'cheatsheet)

(global-set-key (kbd "C-c C-c") 'cheatsheet-show)
(global-set-key (kbd "C-c C-g") 'cheatsheet-get)

;; general cheatsheets
(cheatsheet-add-group 'General
		      '(:key "Control" :description "works as C key")
		      '(:key "Cmd" :description "works as M key")
		      '(:key "Shift" :description "works as S key")
		      '(:key "Space bar" :description "works as SPC key")
		      '(:key "C-<tab>" :description "switch to next buffer")
		      '(:key "C-S-<tab>" :description "switch to previous buffer")
		      '(:key "C-c C-c" :description "opens the cheatsheet")
		      )

;; cheatsheets for hydras
(cheatsheet-add-group 'Hydra
		'(:key "C-c =" :description "opens zoom hydra")
		'(:key "C-c g" :description "opens magit hydra")
		'(:key "C-c f" :description "opens file operations hydra")
		'(:key "C-o" :description "opens window switching hydra")
		'(:key "C-c w" :description "open window operations hydra")
		'(:key "C-x SPC" :description "opens rectangle operations hydra")
		)
