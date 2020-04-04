;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Reuben Steenekamp"
      user-mail-address "reuben.steenekamp@smsportal.com")

(setq doom-font (font-spec :family "monoid" :size 10))
(setq doom-theme 'doom-one)

(setq magit-git-executable "C:\\Program Files\\Git\\cmd\\git.exe")
(defvar git-bash-executable "C:\\Program Files\\Git\\git-bash.exe")
(defun git-bash ()
  (interactive)
  (start-process "git-bash" nil git-bash-executable))

(defun cmd ()
  (interactive)
  (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
    (set-process-query-on-exit-flag proc nil)))

(after! org
  (setq org-directory "~/org/")
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

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i" :prepend t))))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(after! web-mode
  ;(add-hook 'web-mode-hook #'lsp)
  (setq web-mode-code-indent-offset 2))

(after! editorconfig
  (editorconfig-mode 1))

(after! magit
  (require 'forge))

(after! lsp-haskell
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '())
  (setq lsp-log-io t)
  )

(map!
 :leader

 (:desc "M-x"
   "<SPC>"
   #'counsel-M-x)

 (:desc "Open git bash"
   "oog"
   #'git-bash)
 (:desc "Open cmd"
   "ooc"
   #'cmd)

 (:desc "Evil no highlight"
   "sc"
   #'evil-ex-nohighlight))

(evil-snipe-mode 0)
