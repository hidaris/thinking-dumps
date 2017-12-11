;;; (empty-stack) = []
;;; (push v [v1 v2 v3 ...]) = [v v1 v2 v3 ...]
;;; (pop v [v v1 v2 v3 ...]) = [v1 v2 v3]
;;; (top [v1 v2 v3 ...]) = v
;;; (empty-stack? stack) = #t if stack = [], else #f

;;; constructors : empty-stack push
;;; observers : pop, top, empty-stack?
