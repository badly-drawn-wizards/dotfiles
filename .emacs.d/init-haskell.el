(require 'use-package)

(use-package ghc
  :config (progn
	    (add-hook 'haskell-mode-hook 'ghc-init))
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config (progn
	    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
	    (setq haskell-indentation-show-indentations nil)
	    (setq haskell-process-type 'cabal-repl))
	    (setq haskell-notify-p t)
	    (setq haskell-tags-on-save t)
	    (setq haskell-stylish-on-save t))

