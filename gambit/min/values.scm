;;; Copyright (c) 1994-2020 by Marc Feeley, All Rights Reserved.

;;; Values.

(define-prim (##make-values len #!optional (init 0))
  (let ((vals (##make-vector len init)))
    (##subtype-set! vals (macro-subtype-boxvalues))
    vals))

(define-prim (##values-length vals)
  (##vector-length vals))

(define-prim (##values-ref vals i)
  (##vector-ref vals i))

(define-prim (##values-set! vals i val)
  (##vector-set! vals i val))

(define-prim (##list->values lst)
  (let loop1 ((x lst) (n 0))
    (if (##pair? x)
        (loop1 (##cdr x) (##fx+ n 1))
        (let ((vals (##make-values n)))
          (let loop2 ((x lst) (i 0))
            (if (and (##pair? x)  ;; double check in case another
                     (##fx< i n)) ;; thread mutates the list
                (let ((elem (##car x)))
                  (##values-set! vals i elem)
                  (loop2 (##cdr x) (##fx+ i 1)))
                vals))))))

(define-prim (##values->list vals)
  (let ((start 0)
        (end (##values-length vals)))
    (let loop ((lst '()) (i (##fx- end 1)))
      (if (##fx< i start)
          lst
          (loop (##cons (##values-ref vals i) lst) (##fx- i 1))))))

(define-prim (##values
              #!optional
              (val1 (macro-absent-obj))
              (val2 (macro-absent-obj))
              (val3 (macro-absent-obj))
              #!rest
              others)
  (cond ((##eq? val2 (macro-absent-obj))
         (if (##eq? val1 (macro-absent-obj))
             (##values)
             val1))
        ((##eq? val3 (macro-absent-obj))
         (##values val1 val2))
        ((##null? others)
         (##values val1 val2 val3))
        (else
         (##list->values
          (##cons val1 (##cons val2 (##cons val3 others)))))))

(define-prim (values
              #!optional
              (val1 (macro-absent-obj))
              (val2 (macro-absent-obj))
              (val3 (macro-absent-obj))
              #!rest
              others)
  (cond ((##eq? val2 (macro-absent-obj))
         (if (##eq? val1 (macro-absent-obj))
           (##values)
           val1))
        ((##eq? val3 (macro-absent-obj))
         (##values val1 val2))
        ((##null? others)
         (##values val1 val2 val3))
        (else
         (##list->values
          (##cons val1 (##cons val2 (##cons val3 others)))))))

(define-prim (##call-with-values producer consumer)
  (let ((results ;; may get bound to a multiple-values object
         (producer)))
    (if (##not (##values? results))
        (consumer results)
        (let ((len (##values-length results)))
          (cond ((##fx= len 2)
                 (consumer (##values-ref results 0)
                           (##values-ref results 1)))
                ((##fx= len 3)
                 (consumer (##values-ref results 0)
                           (##values-ref results 1)
                           (##values-ref results 2)))
                ((##fx= len 0)
                 (consumer))
                (else
                 (##apply consumer (##values->list results))))))))

(define-prim (call-with-values producer consumer)
  (macro-force-vars (producer consumer)
    (macro-check-procedure producer 1 (call-with-values producer consumer)
      (macro-check-procedure consumer 2 (call-with-values producer consumer)
        (##call-with-values producer consumer)))))
