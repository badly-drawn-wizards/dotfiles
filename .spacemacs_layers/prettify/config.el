(defvar prettify-symbols-table-alist
  '((greek-symbols
     ("alpha" ?α)
     ("beta" ?β)
     ("gamma" ?γ)
     ("delta" ?δ)
     ("epsilon" ?ε)
     ("zeta" ?ζ)
     ("eta" ?η)
     ("theta" ?θ)
     ("iota" ?ι)
     ("kappa" ?κ)
     ("lambda" ?λ)
     ("mu" ?μ)
     ("nu" ?ν)
     ("xi" ?ξ)
     ("omicron" ?ο)
     ("pi" ?π)
     ("rho" ?ρ)
     ("sigma" ?Σ)
     ("upsilon" ?υ)
     ("phi" ?φ)
     ("chi" ?χ)
     ("psi" ?ψ)
     ("omega" ?ω))
    (equality-symbols
     (":=" ?≔)
     ("<>" ?≠)
     ("!=" ?≠)
     ("<=" ?≤)
     (">=" ?≥))
    (quantifier-symbols
     ("forall" ?∀)
     ("exists" ?∃))
    (arrow-symbols
     ("->" ?→)
     ("<-" ?←)
     ("<->" ?↔)
     ("=>" ?⇒))
    (truth-symbols
     ("True" ?⊤)
     ("False" ?⊥))
    (bool-symbols
     ("true" ?⊤)
     ("false" ?⊥))
    (logic-symbols
     ("~" ?¬)
     ("/\\" ?∧)
     ("\\/" ?∨))
    (lambda-symbols
     ("lambda" ?λ)
     ("fun" ?λ)
     ("function" ?λ))
    (set-symbols
     ("bool" ?𝔹)
     ("nat" ?ℕ)
     ("Z" ?ℤ)
     ("Q" ?ℚ)
     ("R" ?ℝ)
     )
    (coq-symbols
     ("||" ?∥)
     ("|-" ?⊢))))

(defvar default-symbols '(lambda-symbols equality-symbols arrow-symbols))

(defvar prettify-symbols-major-mode-alist '())

(defun get-prettify-symbols-alist-for-mode (mode)
  (let ((table-key-list (or (assoc mode prettify-symbols-major-mode-alist) default-symbols)))
    (apply #'append (mapcar (lambda (table-key) (cdr (assoc table-key prettify-symbols-table-alist))) table-key-list))
  ))

(defun update-prettify-symbol ()
  (setq prettify-symbols-alist (get-prettify-symbols-alist-for-mode major-mode))
  (prettify-symbols-mode t))

(add-hook 'after-change-major-mode-hook 'update-prettify-symbol)
