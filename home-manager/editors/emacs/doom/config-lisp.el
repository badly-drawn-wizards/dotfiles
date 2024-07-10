;;; config-lisp.el --- Description -*- lexical-binding: t; -*-
;;

(provide 'config-lisp)
;;; config-lisp.el ends here

(after! parinfer-rust
  (setq parinfer-rust-preferred-mode "smart"))

(map!
  :leader
  (:desc "Evil no highlight" "sc" #'evil-ex-nohighlight)
  (:desc "Smartparens" "k")
  ("kr" #'sp-raise-sexp)
  ("ks" #'sp-forward-slurp-sexp)
  ("kS" #'sp-backward-slurp-sexp)
  ("kb" #'sp-forward-barf-sexp)
  ("kB" #'sp-backward-barf-sexp))
