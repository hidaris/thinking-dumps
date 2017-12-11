;; (value-of rator p σ₀) = (rator-val σ₁)
;; (value-of rand p σ₁) = (rand-val σ₂)
;; --------------------------------------------------------
;; (value-of (call-exp rator rand) p σ₀)
;; = (apply-procedure (expval->proc rator-val) rand-val σ₂)
