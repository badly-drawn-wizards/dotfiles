(require 'use-package)
(use-package company
  :ensure t
  :init (setq company-idle-delay 0.1)
  :bind ("M-/" . company-complete))

(global-company-mode 1)
