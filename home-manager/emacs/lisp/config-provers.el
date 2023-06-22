;;; config-provers.el --- Description -*- lexical-binding: t; -*-
;;;

(require 'lean4-mode)
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

(provide 'config-provers)
;;; config-lean4.el ends here
