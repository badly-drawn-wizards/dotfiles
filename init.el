(defun config (x) (load (concat "~/.emacs.d/" x)))

(config "init-package")

(config "init-server")
(config "init-company")
(config "init-magit")
(config "init-evil")
(config "init-idris")
(config "init-theme")
(config "init-popwin")
(config "deinit-annoyances")
