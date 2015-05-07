(require 'use-package)
(use-package company
	     :init (progn
		     (global-company-mode 1)
		     (setq company-idle-delay 0.1))
	     :bind ("M-/" . company-complete))
