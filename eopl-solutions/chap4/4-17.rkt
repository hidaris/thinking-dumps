;; (value-of (proc (var₁ var₂ ... var_n) body) env store)
;; ---------------------------------------------------------------
;; ((proc-val (procedure (var₁ var₂ ... var_n) body env)) . store)

;; (value-of ((proc (var₁ var₂ ... var_n) body) (exp1 exp2 ... expn)) env store)
;; (value-of (exp1 exp2 ... expn) env store) => ((val1 val2 ... val3) . store)
;; -----------------------------------------------------------------------------
;; (value-of body (extend-env* (var1 var2 ... varn) (l1 l2 ... ln) env)
;;                [l1 val1] [l2 val2]... [ln valn]store)

;; (value-of ((let (var₁ var₂ ... var_n) (exp1 exp2 ... expn) body)) env store)
;; (value-of (exp1 exp2 ... expn) env store) => ((val1 val2 ... val3) . store)
;; -----------------------------------------------------------------------------
;; (value-of body (extend-env* (var1 var2 ... varn) (l1 l2 ... ln) env)
;;                [l1 val1] [l2 val2]... [ln valn]store)
