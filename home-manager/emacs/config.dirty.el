;;;  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-extra)
(require 'lean-mode)
(require 'ht)
(require 'org)
(require 'org-clock)

(setq user-full-name "Reuben Steenekamp"
      user-mail-address "reuben.steenekamp@gmail.com")

(setq doom-theme 'doom-spacegrey)

(defmacro with-widen (&rest body)
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defmacro with-marker (marker &rest body)
  (let ((m (cl-gensym))
        (b (cl-gensym)))
    `(when-let
      ((,m ,marker)
       (,b (marker-buffer ,m)))
      (with-current-buffer ,b
        (with-widen
          (goto-char ,m)
          ,@body)))))

(defun my/org-clock-elapsed-clock-in-time ()
  (floor
   (org-time-convert-to-integer
    (if (and (org-clock-is-active) (= org-clock-marker (point-marker))) (org-time-since org-clock-start-time) 0))
   60))

(defun my/org-clock-get-clocked-time ()
  (+
   (org-clock-sum-current-item
             (org-clock-get-sum-start))
   (my/org-clock-elapsed-clock-in-time)))

(defun my/org-marker-heading (marker)
  (with-marker marker (org-no-properties (org-get-heading t t t t))))

(defun my/org-clock-info ()
  (json-serialize
   (let ((active (when (org-clock-is-active) 't)))
    (or
      (when-let
        ((task (if active org-clock-marker (car org-clock-history)))
         (task-buffer (marker-buffer task)))
        (ht
          ("active" (or active :false))
          ("heading"
           (or (my/org-marker-heading task) :null))
          ("clockedMinutes"
           (or (with-marker task (my/org-clock-elapsed-clock-in-time)) :null))
          ("totalMinutes"
           (or (with-marker task (my/org-clock-get-clocked-time)) :null))
          ("recent"
           (map 'array #'my/org-marker-heading org-clock-history))
          ("buffer"
           (buffer-file-name task-buffer))
          ("point"
           (marker-position task))))
      :null))))

(defun my/org-clock-info-clock-in (i)
  (with-marker (nth i org-clock-history) (org-clock-in nil nil)))

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

(after! parinfer-rust
  (setq parinfer-rust-preferred-mode "smart"))

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

(require 'lean4-input)
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

(after! prettier-js
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

(map!
  :leader

  (:desc "M-x" "<SPC>" #'counsel-M-x)

  (:desc "Comment" ";" #'evilnc-comment-operator)

  ;; TODO Figure out why this doesn't work
  (:desc "jump" "j")
  ("jj" #'avy-goto-char)
  ("jl" #'avy-goto-line)

  (:desc "Evil no highlight" "sc" #'evil-ex-nohighlight)
  (:desc "Smartparens" "k")
  ("kr" #'sp-raise-sexp)
  ("ks" #'sp-forward-slurp-sexp)
  ("kS" #'sp-backward-slurp-sexp)
  ("kb" #'sp-forward-barf-sexp)
  ("kB" #'sp-backward-barf-sexp))
 
(setq tab-width 2)
(setq evil-shift-round t)
(setq evil-shift-width 2)
(setq evil-snipe-mode 0)

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

(defcustom nix-fetchers-list '("fetchurl" "fetchgit" "fetchFromGitHub")
  "List of nix fetchers for selecting fetcher at point"
  :type '(list string))

;;;###autoload
(defun nix-prefetch ()
  (interactive)
  (unless mark-active
    (unless (nix-a-fetcher) (user-error "Could not find fetcher to prefetch")))
  (if mark-active
      (progn
        (goto-char (1+ (point)))
        (nix-prefetch-region)
        (setq mark-active nil))
      (error "Impossible: mark not active should be unreachable here")))

;;;###autoload
(defun nix-a-fetcher (&optional dir)
  (interactive)
  (if dir
   (let ((re (eval `(rx (or ,@nix-fetchers-list))))
         (prev (point)))
     (condition-case _
       (progn
         (cl-case dir
           ('forward (search-forward-regexp re))
           ('backward (search-backward-regexp re)))
         (let ((start (match-beginning 0))
               (end
                 (progn
                   (search-forward "{")
                   (goto-char (1+ (point)))
                   (end-of-thing 'list)
                   (goto-char (1- (point)))
                   (point))))
           (set-mark start)
           (goto-char end)
           (setq mark-active 't)
           't))
       ((search-failed user-error)
        (goto-char prev)
        nil)))
   (let ((prev (point))
         (backwards (nix-a-fetcher 'backward)))
     (if
       (and
        backwards
        (<= (mark) prev)
        (<= prev (point)))
       't
       (progn
         (goto-char prev)
         (nix-a-fetcher 'forward))))))
    

;;;###autoload
(defun nix-prefetch-region (&optional start end)
  (interactive (list (region-beginning) (region-end)))
  (unless start (setq start (region-beginning)))
  (unless end (setq end (region-end)))
  (message "region %d %d" start end)
  (if-let* ((region (buffer-substring start end))
            (args (list "--output" "nix" "-s" "-E" region))
            (replacement
              (with-temp-buffer
                (let ((exit-code (apply 'call-process "nix-prefetch" nil (current-buffer) nil args)))
                  (if (= 0 exit-code)
                    (progn
                      (replace-regexp-in-string
                        "\n\\'" "" (buffer-string)))
                    (progn
                      (message "region was:\n%s" region)
                      (message "nix-prefetch-region failed with:\n%s" (buffer-string))
                      nil))))))
      (save-excursion
        (goto-char start)
        (if (re-search-forward "{[^}]+}" end 't)
            (progn
              (replace-match replacement)
              (indent-region start (point)))))))
