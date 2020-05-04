;; (value-of exp_1 p₁ σ₀) = (val₁ σ₁)
;; ...
;; (value-of exp_n p_n-1 σ_n-1) = (val_n σ_n)
;; ---------------------------------------
;; (value-of (begin-exp rator rand) p σ₀)
;; = (value-of exp_n p_n-1 σ_n-1) => exp_n
;; = val_n
