;; (value-of ((letrec (var₁ var₂ ... var_n)
;;                    (exp1 exp2 ... expn) body)) env store)
;; (value-of (exp1 exp2 ... expn) env store)
;;  => ((procval1 procval2 ... procval3) . store)
;; ----------------------------------------------------------------------
;; (value-of body (extend-env* (var1 var2 ... varn) (l1 l2 ... ln) env)
;;                [l1 procval1] [l2 procval2]... [ln procvaln]store)
