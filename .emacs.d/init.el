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

;;;;;;;;;;;;;
;; Spacing ;;
;;;;;;;;;;;;;

(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

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
	(flycheck-pos-tip . "melpa")
	(evil-org . "melpa-stable")
	(ghc . "melpa-stable")
	(haskell-mode . "melpa-stable")
	(company-ghc . "melpa-stable")
	(clojure-mode . "melpa-stable")
	(inf-clojure . "melpa-stable")
	(rust-mode . "melpa")
	(racer . "melpa")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment and Path ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'exec-path "~/.local/bin")

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
	      "u" 'universal-argument
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
  :config (progn
	    (helm-mode 1))
  :ensure t)

;;;;;;;;;;;;;
;; Linting ;;
;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :init (flycheck-pos-tip-mode))

;;;;;;;;;;;;;;
;; Org mode ;;
;;;;;;;;;;;;;;

(use-package org
  :init (progn
	 (setq org-directory "~/org")
	 (setq org-mobile-directory "~/mobile-org")

	 (setq org-log-done 'time)

	 (setq org-agenda-files (concat org-directory "/agenda-files.txt"))
	 (setq org-refile-targets '((nil :maxlevel . 9)
				    (org-agenda-files :maxlevel . 9)))

	 (setq org-default-notes-file (concat org-directory "/notes.org"))

	 (setq org-link-frame-setup
	       '((vm . vm-visit-folder-other-frame)
		 (vm-imap . vm-visit-imap-folder-other-frame)
		 (gnus . org-gnus-no-new-news)
		 (file . find-file)
		 (wl . wl-other-frame)))

	 (org-babel-do-load-languages
	  'org-babel-load-languages
	  '((haskell . t)
	    (sh . t)
	    (latex . t)))
	 (add-to-list 'org-entities-user
		      '("vdash" "\\vdash" nil nil nil nil "⊢"))))

(use-package evil-org
  :ensure t)

;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;

;; Haskell

(use-package ghc
  :ensure t
  :config (progn
	    (defun ghc-init-fun () (ghc-init))
	    (add-hook 'haskell-mode-hook 'ghc-init-fun)))

(use-package haskell-mode
  :ensure t
  :config (progn
	    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

	    (require 'haskell-indentation)
	    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

	    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	    (setq haskell-process-type 'stack-ghci)
	    (setq haskell-process-path-ghci "stack")
	    (setq haskell-process-args-ghci "ghci")

	    (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

	    ;; (defun flycheck-haskell-stack-init () (flycheck-select-checker 'haskell-stack-ghc))
	    ;; (add-hook 'haskell-mode-hook 'flycheck-haskell-stack-init)

	    (setq haskell-notify-p t)
	    (setq haskell-tags-on-save t)
	    (setq haskell-stylish-on-save t)))

(use-package company-ghc
  :ensure t
  :config (add-to-list 'company-backends 'company-ghc))

;; Clojure

(use-package clojure-mode
  :ensure t)

(use-package inf-clojure
  :ensure t)

;; Rust

(use-package rust-mode
  :ensure t)

(use-package racer
  :init (progn
	  (add-hook 'rust-mode-hook #'racer-mode)
	  (add-hook 'racer-mode-hook #'eldoc-mode))
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
	  (setq proof-splash-enable nil)
	  (setq proof-script-fly-past-comments t)
	  (setq proof-three-window-mode-policy 'hybrid)
	  ))

;; Coq

(use-package company-coq
  :init (progn
	  (add-hook 'coq-mode-hook #'company-coq-initialize))
  :ensure t)

;;; init.el ends here
