;;; config-envrc.el --- Description -*- lexical-binding: t; -*-
;;

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

(after! envrc
  (defadvice envrc--update (after envrc-hooks () activate)
    (run-hooks 'envrc-hook)))

(provide 'config-envrc)
;;; config-envrc.el ends here
