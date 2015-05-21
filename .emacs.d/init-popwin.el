(require 'use-package)

(use-package popwin
  :ensure t
  :config (progn
	  (require 'popwin)
	  (push 'idris-compiler-notes-mode
		popwin:special-display-config)
	  (push '(idris-repl-mode :height 0.2 :noselect nil :position bottom :stick t)
		popwin:special-display-config)
	  (popwin-mode 1)))
