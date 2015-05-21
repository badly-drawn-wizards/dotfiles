(require 'use-package)
(use-package evil
  :ensure t
  :config (evil-mode 1))
(use-package evil-escape
  :ensure t
  :config (progn
	    (require 'hl-line)
	    (evil-escape-mode 1)))
