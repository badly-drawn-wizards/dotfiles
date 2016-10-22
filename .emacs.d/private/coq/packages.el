(setq coq-packages
    '(
      company-coq
      (proof-general :location local)
      ))

(setq coq-excluded-packages '())

(defun coq/init-proof-general ()
  "Initialize Proof General"
  (use-package proof-site
    :defer t
    :mode ("\\.v\\'" . coq-mode)
    :init (progn
            (setq proof-splash-enable nil)
            (setq proof-script-fly-past-comments t)
            (setq proof-three-window-mode-policy 'hybrid)
            )
    :config (progn
              (spacemacs/set-leader-keys-for-major-mode 'coq-mode
                "." 'proof-goto-point
                "n" 'proof-assert-next-command-interactive
                "p" 'proof-undo-last-successful-command
                ))
    ))

(defun coq/init-company-coq ()
  (use-package company-coq
    :defer t
    :config
    (add-hook 'coq-mode-hook #'company-coq-mode)))

