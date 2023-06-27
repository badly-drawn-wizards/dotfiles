;;; config-org.el --- Description -*- lexical-binding: t; -*-

(defvar my/org-drill-file nil)
(after! org
  (setq org-directory (concat (getenv "HOME") "/org"))
  (setq org-agenda-files (list org-directory))
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

(provide 'config-org)
;;; config-org.el ends here
