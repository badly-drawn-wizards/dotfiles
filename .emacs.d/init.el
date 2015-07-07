;;;;;;;;;;;;;;;;
;; Annoyances ;;
;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(toggle-tool-bar-mode-from-frame 0)
(toggle-menu-bar-mode-from-frame 0)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-pinned-packages
      '(
	(powerline . "melpa-stable")
	(moe-theme . "melpa")
	(magit . "melpa-stable")
	(evil . "melpa-stable")
	(evil-leader . "melpa-stable")
	(company . "melpa-stable")
	(flycheck . "melpa-stable")
	(ghc . "melpa-stable")
	(haskell-mode . "melpa-stable")
	(scala-mode2 . "melpa-stable")
	(sbt-mode . "melpa")
	(ensime . "melpa")
	(idris-mode . "melpa-stable")))

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;;;;;;;;;;;;;;;;
;; Aesthetics ;;
;;;;;;;;;;;;;;;;

;; (use-package solarized-theme
;;   :ensure t)

(use-package powerline
  :ensure t)

(use-package moe-theme
  :ensure t
  :config (progn
	    (load-theme 'moe-dark t)
	    (powerline-moe-theme)
	    (moe-theme-random-color)))

;;;;;;;;;;;;;;;;;;;
;; Vim Emulation ;;
;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package evil-leader
  :ensure t
  :config (progn
	    (evil-leader/set-leader "<SPC>")
	    (evil-leader/set-key
	      "s" 'ghc-case-split
	      "c" 'comment-or-uncomment-region
	      "a" 'align-regexp)
	    (global-evil-leader-mode)))

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init (setq company-idle-delay 0.1)
  :config (global-company-mode 1)
  :bind ("M-/" . company-complete))

;;;;;;;;;;;;;
;; Linting ;;
;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;
;; Versioning ;;
;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :config (progn
	    (setq magit-last-seen-setup-instructions "1.4.0")))

;;;;;;;;;;;;
;; Server ;;
;;;;;;;;;;;;

;; (require 'server)

;; (unless (server-running-p)
;;   (server-start))

;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;

;; Haskell

(use-package ghc
  :ensure t
  :config (progn
	    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))

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

;; Scala

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

;; Idris

(use-package idris-mode
  :ensure t)
