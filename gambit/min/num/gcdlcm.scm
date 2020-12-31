;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; gcd, lcm

(define-prim (##gcd2 x y)

  (##define-macro (type-error-on-x) `'(1))
  (##define-macro (type-error-on-y) `'(2))

  (define (fast-gcd u v)

    ;; See the paper "Fast Reduction and Composition of Binary
    ;; Quadratic Forms" by Arnold Schoenhage.  His algorithm and proof
    ;; are derived from, and basically the same for, his Controlled
    ;; Euclidean Descent algorithm for gcd, which he has never
    ;; published.  This algorithm has complexity log N times a
    ;; constant times the complexity of a multiplication of the same
    ;; size.  We don't use it until we get to about 6800 bits.  Note
    ;; that this is the same place that we start using FFT
    ;; multiplication and fast division with Newton's method for
    ;; finding inverses.

    ;; Niels Mo"ller has written two papers about an improved version
    ;; of this algorithm.

    ;; assumes u and v are nonnegative exact ints

    (define (make-gcd-matrix A_11 A_12
                             A_21 A_22)
      (##vector A_11 A_12
                A_21 A_22))

    (define (gcd-matrix_11 A)
      (##vector-ref A 0))

    (define (gcd-matrix_12 A)
      (##vector-ref A 1))

    (define (gcd-matrix_21 A)
      (##vector-ref A 2))

    (define (gcd-matrix_22 A)
      (##vector-ref A 3))

    (define (make-gcd-vector v_1 v_2)
      (##vector v_1 v_2))

    (define (gcd-vector_1 v)
      (##vector-ref v 0))

    (define (gcd-vector_2 v)
      (##vector-ref v 1))

    (define gcd-matrix-identity '#(1 0
                                   0 1))

    (define (gcd-matrix-multiply A B)
      (cond ((##eq? A gcd-matrix-identity)
             B)
            ((##eq? B gcd-matrix-identity)
             A)
            (else
             (let ((A_11 (gcd-matrix_11 A)) (A_12 (gcd-matrix_12 A))
                   (A_21 (gcd-matrix_21 A)) (A_22 (gcd-matrix_22 A))
                   (B_11 (gcd-matrix_11 B)) (B_12 (gcd-matrix_12 B))
                   (B_21 (gcd-matrix_21 B)) (B_22 (gcd-matrix_22 B)))
               (make-gcd-matrix (##+ (##* A_11 B_11)
                                     (##* A_12 B_21))
                                (##+ (##* A_11 B_12)
                                     (##* A_12 B_22))
                                (##+ (##* A_21 B_11)
                                     (##* A_22 B_21))
                                (##+ (##* A_21 B_12)
                                     (##* A_22 B_22)))))))

    (define (gcd-matrix-multiply-strassen A B)
      ;; from http://mathworld.wolfram.com/StrassenFormulas.html
      (cond ((##eq? A gcd-matrix-identity)
             B)
            ((##eq? B gcd-matrix-identity)
             A)
            (else
             (let ((A_11 (gcd-matrix_11 A)) (A_12 (gcd-matrix_12 A))
                   (A_21 (gcd-matrix_21 A)) (A_22 (gcd-matrix_22 A))
                   (B_11 (gcd-matrix_11 B)) (B_12 (gcd-matrix_12 B))
                   (B_21 (gcd-matrix_21 B)) (B_22 (gcd-matrix_22 B)))
               (let ((Q_1 (##* (##+ A_11 A_22) (##+ B_11 B_22)))
                     (Q_2 (##* (##+ A_21 A_22) B_11))
                     (Q_3 (##* A_11 (##- B_12 B_22)))
                     (Q_4 (##* A_22 (##- B_21 B_11)))
                     (Q_5 (##* (##+ A_11 A_12) B_22))
                     (Q_6 (##* (##- A_21 A_11) (##+ B_11 B_12)))
                     (Q_7 (##* (##- A_12 A_22) (##+ B_21 B_22))))
                 (make-gcd-matrix (##+ (##+ Q_1 Q_4) (##- Q_7 Q_5))
                                  (##+ Q_3 Q_5)
                                  (##+ Q_2 Q_4)
                                  (##+ (##+ Q_1 Q_3) (##- Q_6 Q_2))))))))

    (define (gcd-matrix-solve A y)
      (let ((y_1 (gcd-vector_1 y))
            (y_2 (gcd-vector_2 y)))
        (make-gcd-vector (##- (##* y_1 (gcd-matrix_22 A))
                              (##* y_2 (gcd-matrix_12 A)))
                         (##- (##* y_2 (gcd-matrix_11 A))
                              (##* y_1 (gcd-matrix_21 A))))))

    (define (x>=2^n x n)
      (##fx< n (##integer-length x)))

    (define (determined-minimal? u v s)
      ;; assumes  2^s <= u , v; s>= 0 fixnum
      ;; returns #t if we can determine that |u-v|<2^s
      ;; at least one of u and v is a bignum
      (let ((u (if (##fixnum? u) (##fixnum->bignum u) u))
            (v (if (##fixnum? v) (##fixnum->bignum v) v)))
        (let ((u-length (##bignum.mdigit-length u)))
          (and (##fx= u-length (##bignum.mdigit-length v))
               (let loop ((i (##fx- u-length 1)))
                 (let ((v-digit (##bignum.mdigit-ref v i))
                       (u-digit (##bignum.mdigit-ref u i)))
                   (if (and (##fxzero? u-digit)
                            (##fxzero? v-digit))
                       (loop (##fx- i 1))
                       (and (##fx= (##fxquotient s ##bignum.mdigit-width)
                                   i)
                            (##fx< (##fxmax (##fx- u-digit v-digit)
                                            (##fx- v-digit u-digit))
                                   (##fxarithmetic-shift-left
                                    1
                                    (##fxremainder s ##bignum.mdigit-width)))))))))))

    (define (gcd-small-step cont M u v s)
      ;;  u, v >= 2^s
      ;; M is the matrix product of the partial sums of
      ;; the continued fraction representation of a/b so far
      ;; returns updated M, u, v, and a truth value
      ;;  u, v >= 2^s and
      ;; if last return value is #t, we know that
      ;; (- (max u v) (min u v)) < 2^s, i.e, u, v are minimal above 2^s

      (define (gcd-matrix-multiply-low M q)
        (let ((M_11 (gcd-matrix_11 M))
              (M_12 (gcd-matrix_12 M))
              (M_21 (gcd-matrix_21 M))
              (M_22 (gcd-matrix_22 M)))
          (make-gcd-matrix (##+ M_11 (##* q M_12))  M_12
                           (##+ M_21 (##* q M_22))  M_22)))

      (define (gcd-matrix-multiply-high M q)
        (let ((M_11 (gcd-matrix_11 M))
              (M_12 (gcd-matrix_12 M))
              (M_21 (gcd-matrix_21 M))
              (M_22 (gcd-matrix_22 M)))
          (make-gcd-matrix M_11  (##+ (##* q M_11) M_12)
                           M_21  (##+ (##* q M_21) M_22))))

      (if (or (##bignum? u)
              (##bignum? v))

          ;; if u and v are nearly equal bignums, the two ##<
          ;; following this condition could take O(N) time to compute.
          ;; When this happens, however, it will be likely that
          ;; determined-minimal? will return true.

          (cond ((determined-minimal? u v s)
                 (cont M
                       u
                       v
                       #t))
                ((##< u v)
                 (let* ((qr (##exact-int.div v u))
                        (q (macro-qr-q qr))
                        (r (macro-qr-r qr)))
                   (cond ((x>=2^n r s)
                          (cont (gcd-matrix-multiply-low M q)
                                u
                                r
                                #f))
                         ((##eqv? q 1)
                          (cont M
                                u
                                v
                                #t))
                         (else
                          (cont (gcd-matrix-multiply-low M (##- q 1))
                                u
                                (##+ r u)
                                #t)))))
                ((##< v u)
                 (let* ((qr (##exact-int.div u v))
                        (q (macro-qr-q qr))
                        (r (macro-qr-r qr)))
                   (cond ((x>=2^n r s)
                          (cont (gcd-matrix-multiply-high M q)
                                r
                                v
                                #f))
                         ((##eqv? q 1)
                          (cont M
                                u
                                v
                                #t))
                         (else
                          (cont (gcd-matrix-multiply-high M (##- q 1))
                                (##+ r v)
                                v
                                #t)))))
                (else
                 (cont M
                       u
                       v
                       #t)))
          ;; here u and v are fixnums, so 2^s, which is <= u and v, is
          ;; also a fixnum
          (let ((two^s (##fxarithmetic-shift-left 1 s)))
            (if (##fx< u v)
                (if (##fx< (##fx- v u) two^s)
                    (cont M
                          u
                          v
                          #t)
                    (let ((r (##fxremainder v u))
                          (q (##fxquotient  v u)))
                      (if (##fx>= r two^s)
                          (cont (gcd-matrix-multiply-low M q)
                                u
                                r
                                #f)
                          ;; the case when q is one and the remainder is < two^s
                          ;; is covered in the first test
                          (cont (gcd-matrix-multiply-low M (##fx- q 1))
                                u
                                (##fx+ r u)
                                #t))))
                ;; here u >= v, but the case u = v is covered by the first test
                (if (##fx< (##fx- u v) two^s)
                    (cont M
                          u
                          v
                          #t)
                    (let ((r (##fxremainder u v))
                          (q (##fxquotient  u v)))
                      (if (##fx>= r two^s)
                          (cont (gcd-matrix-multiply-high M q)
                                r
                                v
                                #f)
                          ;; the case when q is one and the remainder is < two^s
                          ;; is covered in the first test
                          (cont (gcd-matrix-multiply-high M (##fx- q 1))
                                (##fx+ r v)
                                v
                                #t))))))))

    (define (gcd-middle-step cont a b h m-prime cont-needs-M?)
      ((lambda (cont)
         (if (and (x>=2^n a h)
                  (x>=2^n b h))
             (MR cont a b h cont-needs-M?)
             (cont gcd-matrix-identity a b)))
       (lambda (M x y)
         (let loop ((M M)
                    (x x)
                    (y y))
           (if (or (x>=2^n x h)
                   (x>=2^n y h))
               ((lambda (cont) (gcd-small-step cont M x y m-prime))
                (lambda (M x y minimal?)
                  (if minimal?
                      (cont M x y)
                      (loop M x y))))
               ((lambda (cont) (MR cont x y m-prime cont-needs-M?))
                (lambda (M-prime alpha beta)
                  (cont (if cont-needs-M?
                            (if (##fx> (##fx- h m-prime) 1024)
                                ;; here we trade off 1 multiplication
                                ;; for 21 additions
                                (gcd-matrix-multiply-strassen M M-prime)
                                (gcd-matrix-multiply          M M-prime))
                            gcd-matrix-identity)
                        alpha
                        beta))))))))

    (define (MR cont a b m cont-needs-M?)
      ((lambda (cont)
         (if (and (x>=2^n a (##fx+ m 2))
                  (x>=2^n b (##fx+ m 2)))
             (let ((n (##fx- (##fxmax (##integer-length a)
                                      (##integer-length b))
                             m)))
               ((lambda (cont)
                  (if (##fx<= m n)
                      (cont m 0)
                      (cont n (##fx- (##fx+ m 1) n))))
                (lambda (m-prime p)
                  (let ((h (##fx+ m-prime (##fxquotient n 2))))
                    (if (##fx< 0 p)
                        (let ((a   (##arithmetic-shift a (##fx- p)))
                              (b   (##arithmetic-shift b (##fx- p)))
                              (a_0 (##extract-bit-field p 0 a))
                              (b_0 (##extract-bit-field p 0 b)))
                          ((lambda (cont)
                             (gcd-middle-step cont a b h m-prime #t))
                           (lambda (M alpha beta)
                             (let ((M-inverse-v_0 (gcd-matrix-solve M (make-gcd-vector a_0 b_0))))
                               (cont (if cont-needs-M? M gcd-matrix-identity)
                                     (##+ (##arithmetic-shift alpha p)
                                          (gcd-vector_1 M-inverse-v_0))
                                     (##+ (##arithmetic-shift beta p)
                                          (gcd-vector_2 M-inverse-v_0)))))))
                        (gcd-middle-step cont a b h m-prime cont-needs-M?))))))
             (cont gcd-matrix-identity
                   a
                   b)))
       (lambda (M alpha beta)
         (let loop ((M M)
                    (alpha alpha)
                    (beta beta)
                    (minimal? #f))
           (if minimal?
               (cont M alpha beta)
               (gcd-small-step loop M alpha beta m))))))

    ((lambda (cont)
       (if (and (use-fast-bignum-algorithms)
                (##bignum? u)
                (##bignum? v)
                (x>=2^n u ##bignum.fast-gcd-size)
                (x>=2^n v ##bignum.fast-gcd-size))
           (MR cont u v ##bignum.fast-gcd-size #f)
           (cont 0 u v)))
     (lambda (M a b)
       (general-base a b))))

  (define (general-base a b)
    (if (##eqv? b 0)
        a
        (let ((r (macro-qr-r (##exact-int.div a  ;; calculate (remainder a b)
                                              b
                                              #f ;; need-quotient?
                                              #f ;; keep-dividend?
                                              ))))
          (if (##fixnum? b)
              (fixnum-base b r)
              (general-base b r)))))

  (define (fixnum-base a b)
    (##declare (not interrupts-enabled))
    (if (##eqv? b 0)
        a
        (let ((a b)
              (b (##fxremainder a b)))
          (if (##eqv? b 0)
              a
              (fixnum-base b (##fxremainder a b))))))

  (define (exact-gcd x y)
    ;; always returns an exact result, even with inexact arguments.
    (let ((x (cond ((##inexact? x)
                    (##inexact->exact (##flabs x)))
                   ((##negative? x)
                    (##negate x))
                   ((##bignum? x)
                    (##bignum.copy x))
                   (else ;; nonnegative fixnum
                    x)))
          (y (cond ((##inexact? y)
                    (##inexact->exact (##flabs y)))
                   ((##negative? y)
                    (##negate y))
                   ((##bignum? y)
                    (##bignum.copy y))
                   (else ;; nonnegative fixnum
                    y))))
      ;; now x and y are newly allocated, so we can overwrite them if
      ;; necessary in general-base
      (cond ((##eqv? x 0)
             y)
            ((##eqv? y 0)
             x)
            ((##fixnum? x)
             (if (##fixnum? y)
                 (fixnum-base x y)
                 (fixnum-base x (##remainder y x))))
            ((##fixnum? y)
             (fixnum-base y (##remainder x y)))
            (else
             (let* ((first-x-bit
                     (##first-bit-set x))
                    (first-y-bit
                     (##first-bit-set y))
                    (shift-x?
                     (##fx> (##fx* 2 first-x-bit) (##integer-length x)))
                    (shift-y?
                     (##fx> (##fx* 2 first-y-bit) (##integer-length y)))
                    (x
                     (if shift-x?
                         (##arithmetic-shift x (##fx- first-x-bit))
                         x))
                    (y
                     (if shift-y?
                         (##arithmetic-shift y (##fx- first-y-bit))
                         y)))
               (if (or shift-x? shift-y?)
                   ;; we've shifted out all the powers of two in at
                   ;; least one argument, so we need to put them back
                   ;; in the gcd.
                   (##arithmetic-shift (##gcd x y)
                                       (##fxmin first-x-bit first-y-bit))
                   (fast-gcd x y)))))))

  (cond ((##not (##integer? x))
         (type-error-on-x))
        ((##not (##integer? y))
         (type-error-on-y))
        ((##eq? x y)
         (##abs x))
        (else
         (if (and (##exact? x) (##exact? y))
             (exact-gcd x y)
             (##exact->inexact (exact-gcd x y))))))

(define-prim-nary (##gcd x y)
  0
  (##abs x)
  (##gcd2 x y)
  macro-force-vars
  macro-no-check)

(define-prim-nary (gcd x y)
  0
  (if (##integer? x) (##abs x) '(1))
  (##gcd2 x y)
  macro-force-vars
  macro-no-check
  (##pair? ##fail-check-integer))

(define-prim (##lcm2 x y)

  (##define-macro (type-error-on-x) `'(1))
  (##define-macro (type-error-on-y) `'(2))

  (define (exact-lcm x y)
    (if (or (##eqv? x 0) (##eqv? y 0))
        0
        (##abs (##* (##quotient x (##gcd x y))
                    y))))

  (define (inexact-lcm x y)
    (##exact->inexact
     (exact-lcm (##inexact->exact x)
                (##inexact->exact y))))

  (cond ((##not (##integer? x))
         (type-error-on-x))
        ((##not (##integer? y))
         (type-error-on-y))
        (else
         (if (and (##exact? x) (##exact? y))
             (exact-lcm x y)
             (inexact-lcm x y)))))

(define-prim-nary (##lcm x y)
  1
  (##abs x)
  (##lcm2 x y)
  macro-force-vars
  macro-no-check)

(define-prim-nary (lcm x y)
  1
  (if (##integer? x) (##abs x) '(1))
  (##lcm2 x y)
  macro-force-vars
  macro-no-check
  (##pair? ##fail-check-integer))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
