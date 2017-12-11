;; (value-of exp1 p σ₀) = (val₁ σ₁)
;; (exp->num val₁) = 0
;; --------------------------------
;; (value-of (zero?-exp exp1) p σ₀)
;; = (bool-val #t)


;; (value-of exp1 p σ₀) = (val₁ σ₁)
;; (exp->num val₁) =/= 0
;; --------------------------------
;; (value-of (zero?-exp exp1) p σ₀)
;; = (bool-val #f)
