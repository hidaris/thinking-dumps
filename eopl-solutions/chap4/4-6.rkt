;; (value-of exp₁ ρ σ₀) = (l, σ₁)
;; (value-of exp₂ ρ σ₁) = (val, σ₂)
;; ---------------------------------------------------------
;; (value-of (setref-exp exp₁ exp₂) ρ σ₀) = (val, [l=val]σ₂)
