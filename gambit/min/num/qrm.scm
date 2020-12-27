;; quotient, remainder, modulo

(define-prim (##divide x y kind)

  ;; kind = 0  : floor-remainder
  ;; kind = 1  : floor-quotient
  ;; kind = 2  : floor/
  ;; kind = 4  : truncate-remainder
  ;; kind = 5  : truncate-quotient
  ;; kind = 6  : truncate/
  ;; kind = 8  : modulo
  ;; kind = 12 : remainder
  ;; kind = 13 : quotient

  (define (prim)
    (cond ((##fx= kind 0)  floor-remainder)
          ((##fx= kind 1)  floor-quotient)
          ((##fx= kind 2)  floor/)
          ((##fx= kind 4)  truncate-remainder)
          ((##fx= kind 5)  truncate-quotient)
          ((##fx= kind 6)  truncate/)
          ((##fx= kind 12) remainder)
          ((##fx= kind 13) quotient)
          (else            modulo))) ;; kind = 8

  (define (type-error-on-x)
    (##fail-check-integer 1 (prim) x y))

  (define (type-error-on-y)
    (##fail-check-integer 2 (prim) x y))

  (define (divide-by-zero-error)
    (##raise-divide-by-zero-exception (prim) x y))

  (define (fixnum-fixnum-case x y)
    (if (##fx= y -1) ;; needed because (quotient ##min-fixnum -1) is a bignum

        (let ((k (##fxand kind 3)))
          (if (##fx= k 0)
              0 ;; floor-remainder and truncate-remainder
              (if (##fx= k 1)
                  (##negate x) ;; floor-quotient and truncate-quotient
                  (##values (##negate x) 0)))) ;; floor/ and truncate/

        (if (##fx= (##fxand kind 4) 0)

            ;; floor variants
            (let ((k (##fxand kind 3)))
              (if (##fx= k 0)
                  (##fxmodulo x y) ;; floor-remainder
                  (let* ((q (##fxquotient x y))
                         (m (##fxmodulo x y)))
                    (if (or (##eqv? m 0)
                            (##eq? (##fxnegative? x) (##fxnegative? y)))
                        (if (##fx= k 1)
                            q ;; floor-quotient
                            (##values q ;; floor/
                                      m))
                        (if (##fx= k 1)
                            (##fx- q 1) ;; floor-quotient
                            (##values (##fx- q 1) ;; floor/
                                      m))))))

            ;; truncate variants
            (let ((k (##fxand kind 3)))
              (if (##fx= k 0)
                  (##fxremainder x y) ;; truncate-remainder
                  (if (##fx= k 1)
                      (##fxquotient x y) ;; truncate-quotient
                      (##values (##fxquotient x y) ;; truncate/
                                (##fxremainder x y))))))))

  (define (exact-case x y convert-to-inexact?)

    (define (conv n)
      (if convert-to-inexact? (##exact->inexact n) n))

    (let* ((qr (##exact-int.div x
                                y
                                (##fx> (##fxand kind 3) 0) ;; need-quotient?
                                #t                         ;; keep-dividend?
                                ))
           (q (macro-qr-q qr))
           (r (macro-qr-r qr)))

      (if (and (##fx= (##fxand kind 4) 0)
               (##not (##eqv? r 0))
               (##not (##eq? (##negative? x) (##negative? y))))

          ;; floor variants
          (let ((k (##fxand kind 3)))
            (if (##fx= k 0)
                (conv (##+ r y)) ;; floor-remainder
                (if (##fx= k 1)
                    (conv (##- q 1)) ;; floor-quotient
                    (##values (conv (##- q 1)) ;; floor/
                              (conv (##+ r y))))))

          ;; truncate variants
          (let ((k (##fxand kind 3)))
            (if (##fx= k 0)
                (conv r) ;; truncate-remainder
                (if (##fx= k 1)
                    (conv q) ;; truncate-quotient
                    (##values (conv q) ;; truncate/
                              (conv r))))))))

  (define (inexact-case x y)
    (let ((exact-y (##inexact->exact y)))
      (if (##eqv? exact-y 0)
          (divide-by-zero-error)
          (exact-case (##inexact->exact x) exact-y #t))))

  (macro-number-dispatch y (type-error-on-y)

    (macro-number-dispatch x (type-error-on-x) ;; y = fixnum
      (cond ((##fx= y 0)
             (divide-by-zero-error))
            (else
             (fixnum-fixnum-case x y)))
      (cond ((##fx= y 0)
             (divide-by-zero-error))
            (else
             (exact-case x y #f)))
      (type-error-on-x)
      (if (macro-flonum-int? x)
          (inexact-case x y)
          (type-error-on-x))
      (if (macro-cpxnum-int? x)
          (inexact-case x y)
          (type-error-on-x)))

    (macro-number-dispatch x (type-error-on-x) ;; y = bignum
      (exact-case x y #f)
      (exact-case x y #f)
      (type-error-on-x)
      (if (macro-flonum-int? x)
          (inexact-case x y)
          (type-error-on-x))
      (if (macro-cpxnum-int? x)
          (inexact-case x y)
          (type-error-on-x)))

    (type-error-on-y) ;; y = ratnum

    (macro-number-dispatch x (type-error-on-x) ;; y = flonum
      (if (macro-flonum-int? y)
          (inexact-case x y)
          (type-error-on-y))
      (if (macro-flonum-int? y)
          (inexact-case x y)
          (type-error-on-y))
      (type-error-on-x)
      (if (macro-flonum-int? x)
          (if (macro-flonum-int? y)
              (inexact-case x y)
              (type-error-on-y))
          (type-error-on-x))
      (if (macro-cpxnum-int? x)
          (if (macro-flonum-int? y)
              (inexact-case x y)
              (type-error-on-y))
          (type-error-on-x)))

    (if (macro-cpxnum-int? y) ;; y = cpxnum
        (macro-number-dispatch x (type-error-on-x)
          (inexact-case x y)
          (inexact-case x y)
          (type-error-on-x)
          (if (macro-flonum-int? x)
              (inexact-case x y)
              (type-error-on-x))
          (if (macro-cpxnum-int? x)
              (inexact-case x y)
              (type-error-on-x)))
        (type-error-on-y))))

(define-prim (##floor-remainder x y)
  (##divide x y 0))

(define-prim (floor-remainder x y)
  (macro-force-vars (x y)
    (##divide x y 0)))

(define-prim (##floor-quotient x y)
  (##divide x y 1))

(define-prim (floor-quotient x y)
  (macro-force-vars (x y)
    (##divide x y 1)))

(define-prim (##floor/ x y)
  (##divide x y 2))

(define-prim (floor/ x y)
  (macro-force-vars (x y)
    (##divide x y 2)))

(define-prim (##truncate-remainder x y)
  (##divide x y 4))

(define-prim (truncate-remainder x y)
  (macro-force-vars (x y)
    (##divide x y 4)))

(define-prim (##truncate-quotient x y)
  (##divide x y 5))

(define-prim (truncate-quotient x y)
  (macro-force-vars (x y)
    (##divide x y 5)))

(define-prim (##truncate/ x y)
  (##divide x y 6))

(define-prim (truncate/ x y)
  (macro-force-vars (x y)
    (##divide x y 6)))

(define-prim (##remainder x y)
  (##divide x y 12))

(define-prim (remainder x y)
  (macro-force-vars (x y)
    (##divide x y 12)))

(define-prim (##quotient x y)
  (##divide x y 13))

(define-prim (quotient x y)
  (macro-force-vars (x y)
    (##divide x y 13)))

(define-prim (##modulo x y)
  (##divide x y 8))

(define-prim (modulo x y)
  (macro-force-vars (x y)
    (##divide x y 8)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
