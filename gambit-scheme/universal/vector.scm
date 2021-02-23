(declare (extended-bindings))
(namespace (""))
;; AUTOMAGIC:  vector?, vector-length, vector-ref, vector-set!

(define-prim (##vector . lst) (##list->vector lst))
(define-prim (vector . lst) (##list->vector lst))

(define-prim (##make-vector k #!optional (fill 0))
  (##make-vector k fill))
(define-prim (make-vector arg1 #!optional (arg2 #f))
  (macro-force-vars (arg1 arg2) (##make-vector arg1 arg2)))

(define-prim (##vector-shrink! arg1 arg2))
(define-prim (vector-shrink! arg1 arg2) (##vector-shrink! arg1 arg2))
