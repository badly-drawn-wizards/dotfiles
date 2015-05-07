(defun config (x) (load (concat "~/.emacs.d/" x)))


(config "init-server")
(config "init-package")
(config "init-company")
(config "init-evil")
(config "init-idris")
(config "init-theme")
(config "init-popwin")
(config "deinit-annoyances")
