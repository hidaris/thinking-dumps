;; let times4 = 0
;; in begin
;; set times4 = proc (x)
;; if zero?(x)
;; then 0
;; else -((times4 -(x,1)), -4);
;; (times4 3)
;; end
;;
;; after set, time4 is bound in store, when evaluate time4,
;; we will find it in store, so, we don't need an rec-env
