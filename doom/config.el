;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Code:

(setq user-full-name "Reuben Steenekamp"
      user-mail-address "reuben.steenekamp@smsportal.com")

(setq doom-font (font-spec :family "Fira Code" :size 16))
(setq doom-theme 'doom-spacegrey)

(defconst my/opacity 99)
(set-frame-parameter (selected-frame) 'alpha my/opacity)
(add-to-list 'default-frame-alist `(alpha . ,my/opacity))

(defvar pomodoro-espeak--compliments
  '("you sexy beast"
    "you coding machine"
    "you beautiful mind"
    "now have some fun"
    "have a break"))

(defun pomodoro-espeak/speak (msg)
  (start-process "pomodoro-espeak" nil "espeak" msg))

(defun pomodoro-espeak/pomodoro-over ()
  (let*
      ((compliment (seq-random-elt pomodoro-espeak--compliments))
       (msg (format "Pomodoro is over %s" compliment)))
    (pomodoro-espeak/speak msg)))

(defun pomodoro-espeak/pomodoro-break-over ()
  (pomodoro-espeak/speak "Break is over. Back to work."))


(after! alert
  (setq alert-default-style 'libnotify))

(after! org-pomodoro
  (setq org-pomodoro-play-sounds nil)
  (add-hook 'org-pomodoro-finished-hook #'pomodoro-espeak/pomodoro-over)
  (add-hook 'org-pomodoro-break-finished-hook #'pomodoro-espeak/pomodoro-break-over))

(defvar my/org-drill-file nil)
(after! org
  (setq org-directory "~/org")
  (setq my/org-drill-file (concat org-directory "/drill/main.org"))
  (setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 9)))
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i" :prepend t)

          ("d" "Drill")
          ("dd" "Drill" entry
           (file+headline my/org-drill-file "Drill")
           "* Card :drill: \n\n%?" :prepend t)
          ("dq" "Question" entry
           (file+headline my/org-drill-file "Drill")
           "* Card :drill: \n\n%?\n\n** Answer" :prepend t)))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))


(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (setq web-mode-code-indent-offset 2))

(after! lean-mode
  (set-popup-rule! "\\*Lean Goal\\*"
    :side 'right
    :size 50))

(after! editorconfig
  (editorconfig-mode 1))

(after! dante
  (setq dante-methods '(impure-nix new-build bare-ghci)))

(after! direnv
  (direnv-mode))

(map!
 :leader

 (:desc "M-x"
  "<SPC>"
  #'counsel-M-x)

 (:desc "Comment"
  ";"
  #'evilnc-comment-operator)

 ;; TODO Figure out why this doesn't work
 (:desc "jump"
  "j")
 ("jj"
  #'evil-ace-jump-char-mode)
 ("jl"
  #'evil-ace-jump-line-mode)

 (:desc "Evil no highlight"
  "sc"
  #'evil-ex-nohighlight))

(evil-snipe-mode 0)

(setq tab-width 2)
