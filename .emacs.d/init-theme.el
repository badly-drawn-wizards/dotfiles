(require 'use-package)
(use-package solarized-theme
  :ensure t
 ;; :config (load-theme 'solarized-dark t)
  )
(require 'powerline)
(use-package moe-theme
  :ensure t
  :config (progn
	    (require 'powerline)
	    (load-theme 'moe-dark t)
	    (powerline-moe-theme)
	    (moe-theme-random-color)))
