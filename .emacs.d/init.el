;;; init --- My init file


;;; Commentary:
; This is just to shut flycheck up

;;; Code:

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
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

(setq package-pinned-packages
      '(
	(unicode-fonts . "melpa-stable")
	(powerline . "melpa-stable")
	(moe-theme . "melpa")
	(evil . "melpa-stable")
	(evil-leader . "melpa-stable")
	(company . "melpa-stable")
	(helm . "melpa-stable")
	(flycheck . "melpa-stable")
	(evil-org . "melpa-stable")
	(ghc . "melpa-stable")
	(haskell-mode . "melpa-stable")
	(flycheck-haskell . "melpa-stable")
	(clojure-mode . "melpa-stable")
	(inf-clojure . "melpa-stable")
	(scala-mode2 . "melpa-stable")
	(sbt-mode . "melpa")
	(ensime . "melpa")
	(idris-mode . "melpa-stable")
	(coffee-mode . "melpa")
	(company-coq . "melpa")
	))

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)


;;;;;;;;;;;
;; Fonts ;;
;;;;;;;;;;;

(use-package unicode-fonts
  :init (progn
	  (unicode-fonts-setup)
	  )
  :ensure t)

;;;;;;;;;;;;;;;;
;; Aesthetics ;;
;;;;;;;;;;;;;;;;

;; Arrgh, curse that powerline bug
;; (use-package powerline
;;   :ensure t)

(use-package moe-theme
  :ensure t
  :config (progn
	    (load-theme 'moe-dark t)
	    ;; (powerline-moe-theme)
	    (moe-theme-random-color)))

;;;;;;;;;;;;;;;;;;;
;; Vim Emulation ;;
;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init (progn
	  (setq evil-want-C-u-scroll t)
	  (setq evil-want-C-w-delete t)
	  )
  :config (evil-mode 1))

(use-package evil-leader
  :ensure t
  :config (progn
	    (evil-leader/set-leader "<SPC>")
	    (evil-leader/set-key
	      "c" 'comment-or-uncomment-region)
	    (global-evil-leader-mode)))

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init (setq company-idle-delay 0.1)
  :config (global-company-mode 1)
  :bind ("M-/" . company-complete))

(use-package helm
  :ensure t)

;;;;;;;;;;;;;
;; Linting ;;
;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;
;; Org mode ;;
;;;;;;;;;;;;;;

(use-package org
  :init (progn
	 (setq org-directory "~/org")
	 (setq org-mobile-directory "~/mobile-org")

	 (setq org-log-done 'time)

	 (setq org-agenda-files (concat org-directory "/agenda-files.txt"))

	 (setq org-default-notes-file (concat org-directory "/notes.org"))
	 (setq org-refile-targets '((nil :maxlevel . 9)
				    (org-agenda-files :maxlevel . 9)))

	 (org-babel-do-load-languages
	  'org-babel-load-languages
	  '((haskell . t))
	 ))
  )

(use-package evil-org
  :ensure t)

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
	    (require 'haskell-indentation)
	    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
	    (add-hook 'haskell-mode-hook 'haskell-indentation-disable-show-indentations)
	    (setq haskell-process-type 'cabal-repl))
	    (setq haskell-notify-p t)
	    (setq haskell-tags-on-save t)
	    (setq haskell-stylish-on-save t))

(use-package flycheck-haskell
  :ensure t
  :config (progn
	    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)))

;; Clojure

(use-package clojure-mode
  :ensure t)

(use-package inf-clojure
  :ensure t)

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

;; CoffeeScript

(use-package coffee-mode
  :config (progn
	    (setq coffee-tab-width 4)
	   )
  :ensure t)

(provide 'init)

;; Idris

(use-package idris-mode
  :init (progn
	  (setq idris-mode-path "~/idris/Idris-dev/.cabal-sandbox/bin"))
  :ensure t)

;; Proof General

;; Requires proof general installation
(use-package proof-site
  :init (progn
	  (setq proof-splash-display-screen nil)
	  (setq proof-script-fly-past-comments t)
	  ))

;; Coq

(use-package company-coq
  :init (progn
	  (add-hook 'coq-mode-hook #'company-coq-initialize))
  :ensure t)

;;; init.el ends here
