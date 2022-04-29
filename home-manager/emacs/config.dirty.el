;;;  -*- lexical-binding: t; -*-

(setq user-full-name "Reuben Steenekamp"
      user-mail-address "reuben.steenekamp@gmail.com")

(setq doom-theme 'doom-spacegrey)

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
  (setq org-directory (concat (getenv "HOME") "/org"))
  (setq my/org-drill-file (concat org-directory "/drill/main.org"))
  (setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 9)))
  (setq org-default-notes-file (concat org-directory "/todo.org"))
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

(after! nix-mode
  (setq nix-nixfmt-bin "nixpkgs-fmt"))

(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (setq web-mode-code-indent-offset 2))

(require 'lean-mode)
(setq default-input-method "Lean")

(after! lean4-mode
  (set-lookup-handlers! 'lean4-mode
    :definition #'lean4-find-definition)
  (sp-with-modes 'lean4-mode
    (sp-local-pair "/-" "-/")
    (sp-local-pair "`" "`")
    (sp-local-pair "{" "}")
    (sp-local-pair "«" "»")
    (sp-local-pair "⟨" "⟩")
    (sp-local-pair "⟪" "⟫"))
  (map! :map lean4-mode-map
        :mode lean4-mode
        :localleader
        "g" #'lean4-toggle-show-goal
        "n" #'lean4-toggle-next-error
        "e" #'lean4-execute))

(set-popup-rule! "\\*Lean Goal\\*"
    :side 'right
    :size 50)

(after! coq-mode
  (set-popup-rule! "\\*goals\\*"
    :side 'right
    :vslot 1
    :size 50)
  (set-popup-rule! "\\*response\\*"
    :side 'right
    :vslot 1
    :size 50))

(after! editorconfig
  (editorconfig-mode 1))

(after! dante
  (setq dante-methods '(impure-nix new-build bare-ghci)))

(after! envrc
  (defadvice envrc--update (after envrc-hooks () activate)
    (run-hooks 'envrc-hook)))

(defvar executable-var-alist
  '((lsp-python-ms-executable . "python-language-server")
    (lsp-python-ms-python-executable . "python")
    (lsp-csharp-server-path . "omnisharp")
    (lsp-purescript-server-executable . "purescript-language-server")))

(defun update-executable-vars ()
  "Locally update variables that point to executables by looking up them up in PATH."
  (cl-loop for (var . name) in executable-var-alist
     collect (set (make-local-variable var) (executable-find name))))
(add-hook 'envrc-hook #'update-executable-vars)

(after! agda2
  (add-to-list 'auto-mode-alist '("\\.agda\\'" . agda2-mode))
  (add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode)))

(after! prettier-js
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

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
  #'avy-goto-char)
 ("jl"
  #'avy-goto-line)

 (:desc "Evil no highlight"
  "sc"
  #'evil-ex-nohighlight))

(evil-snipe-mode 0)

(setq tab-width 2)
(setq evil-shift-width 2)

(use-package! tree-sitter
  :init
  (defadvice tsc-dyn-get--download (around tsc-dont-download (&rest arg)))
  :config
  (cl-pushnew (expand-file-name "~/.tree-sitter") tree-sitter-load-path)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq projectile-switch-project-action 'projectile-vc)
(setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)
