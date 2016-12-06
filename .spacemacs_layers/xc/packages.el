(setq xc-packages '(flycheck))
(setq xc-excluded-packages '())

(defun xc/post-init-flycheck ()
  "XC8 flycheck definition"
  (flycheck-def-args-var flycheck-c-xc8-args c-xc8)

  (flycheck-def-option-var flycheck-c-xc8-include-path nil c-xc8
    "A list of include directories for Microchip XC8.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of x8.
"
    :type '(repeat (directory :tag "Include directory"))
    :safe #'flycheck-string-list-p
    :package-version '(flycheck . "0.20"))

  (flycheck-define-checker c-xc8
    "A C syntax checker using Microchip XC8."

    :command ("xc8"
              "--errformat=Error:%f:%l:%c:%s"
              "--warnformat=Warning:%f:%l:%c:%s"
              "--pass1"
              (eval (concat "-O" (make-temp-name (expand-file-name "flycheck" (flycheck-temp-dir-system)))))
              (option-list "-I" flycheck-xc8-include-path concat)
              (eval flycheck-xc8-args)
              source)
    :error-patterns
    ((error line-start
            "Error:" (optional (file-name)) ":" (optional line) ":" (optional column) ":" (message)
            line-end)
     (warning line-start
              "Warning:" (optional (file-name)) ":" (optional line) ":" (optional column) ":" (message) line-end)
     )
    :modes (c-mode))
  (add-to-list 'flycheck-checkers 'c-xc8))
