;; (value-of (let-exp (var exp1 body)) env store)
;; (value-of exp1 env store) = (val store1)
;; ------------------------------------------------------------------
;; (value-of body (extend-env var location env) [location = val]store1)
