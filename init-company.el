(require 'company)

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete)
(setq company-idle-delay 0.1)
