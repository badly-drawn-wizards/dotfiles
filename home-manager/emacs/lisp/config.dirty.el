;;;  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-extra)
(require 'ht)

(setq user-full-name "Reuben Steenekamp"
      user-mail-address "reuben.steenekamp@gmail.com")
(setq doom-theme 'doom-spacegrey)

(require 'pomodoro-espeak)
(require 'org-utils)
(require 'config-org)
(require 'config-provers)
(require 'config-treesitter)
(require 'config-envrc)

(after! alert
  (setq alert-default-style 'libnotify))

(after! nix-mode
  (setq nix-nixfmt-bin "nixpkgs-fmt"))

(after! editorconfig
  (editorconfig-mode 1))

(setq-default tab-width 2)
(after! evil
  (setq-default evil-shift-round t)
  (setq-default evil-shift-width 2))
(after! evil-snipe
  (evil-snipe-mode 0))

(map!
  :leader
  (:desc "M-x" "<SPC>" #'counsel-M-x)
  (:desc "Comment" ";" #'evilnc-comment-operator)
  (:desc "jump" "j")
  ("jj" #'avy-goto-char)
  ("jl" #'avy-goto-line))

(after! projectile
  (setq projectile-switch-project-action 'projectile-dired))
