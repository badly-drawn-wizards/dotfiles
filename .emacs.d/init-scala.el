(require 'use-package)

(use-package scala-mode2
  :init (progn
	  (setq scala-indent:default-run-on-strategy 1)
	  (setq scala-indent:indent-value-expression t)
	  (setq scala-indent:align-parameters t)
	  (setq scala-indent:align-forms t))
  :ensure t)

(use-package sbt-mode
  :ensure t)

(use-package ensime
  :ensure t)
