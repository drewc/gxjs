(define-prim (##number->string n)
  (##inline-host-expression
   "(() => { n = g_scm2host(@1@) ; return g_host2scm(n.toString()); })();" n))

(define-prim number->string ##number->string)
;;;----------------------------------------------------------------------------

;;; Numerical type predicates.
(define-prim (##number? x)
  (##complex? x))

(define-prim (##complex? x)
  (macro-number-dispatch x #f
    #t ;; x = fixnum
    #t ;; x = bignum
    #t ;; x = ratnum
    #t ;; x = flonum
    #t)) ;; x = cpxnum

(define-prim (number? x)
  (macro-force-vars (x)
    (##number? x)))

(define-prim (complex? x)
  (macro-force-vars (x)
    (##complex? x)))

;; (define-prim (##real? x)
;;   (macro-number-dispatch x #f
;;     #t ;; x = fixnum
;;     #t ;; x = bignum
;;     #t ;; x = ratnum
;;     #t ;; x = flonum
;;     (macro-cpxnum-real? x))) ;; x = cpxnum

;; (define-prim (real? x)
;;   (macro-force-vars (x)
;;     (##real? x)))

;; (define-prim (##rational? x)
;;   (macro-number-dispatch x #f
;;     #t ;; x = fixnum
;;     #t ;; x = bignum
;;     #t ;; x = ratnum
;;     (macro-flonum-rational? x) ;; x = flonum
;;     (macro-cpxnum-rational? x))) ;; x = cpxnum

;; (define-prim (rational? x)
;;   (macro-force-vars (x)
;;     (##rational? x)))

;; (define-prim (##integer? x)
;;   (macro-number-dispatch x #f
;;     #t ;; x = fixnum
;;     #t ;; x = bignum
;;     #f ;; x = ratnum
;;     (macro-flonum-int? x) ;; x = flonum
;;     (macro-cpxnum-int? x))) ;; x = cpxnum

;; (define-prim (integer? x)
;;   (macro-force-vars (x)
;;     (##integer? x)))

(define-prim (##exact-integer? x)
  (macro-exact-int? x))

(define-prim (exact-integer? x)
   (macro-force-vars (x)
     (macro-exact-int? x)))

#;(##define-macro (define-js-prim-nary name js-op none one)
  (let ((red (string-append  "((xs) => {
    if (xs.length = 0) {
  return g_scm2host(@3@)
 } else (xs.length = 1) {
  xs = [g_scm2host(@4@), xs[0]]
 };
return xs.reduce((a, b) => a " js-op " b) })(g_scm2host(@1@));")))
 `(define-prim (,name . xs)
    (##inline-host-expression ,red (##list->vector xs)))))



#;(define-prim (##+ . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a + b, 0) })(g_scm2host(@1@));"
   (##list->vector xs)))
#;(define-prim + ##+)

#;(define-prim (##* . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a * b, 1) })(g_scm2host(@1@));"
   (##list->vector xs)))


#;(define-js-prim-nary ##* "*" 1 1)
#;(define-prim * ##*)
#;(define-prim (##- . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a - b) })(g_scm2host(@1@));"
   (##list->vector xs)))
#;(define-prim - ##-)
#;(define-prim (##/ . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a / b, 1) })(g_scm2host(@1@));"
   (##list->vector xs)))
#;(define-prim / ##/)


(define-prim (##= . xs)
  (##inline-host-expression
   "((xs) => {
      const _ret = xs.reduce((a, b) => { return (a == b) ?  b : NaN });
      return !isNaN(_ret)})(g_scm2host(@1@));"
   (##list->vector xs)))
(define-prim = ##=)

(define-prim (##< . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a < b) })(g_scm2host(@1@));"
   (##list->vector xs)))
(define-prim < ##<)
(define-prim (##> . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a > b) })(g_scm2host(@1@));"
   (##list->vector xs)))
(define-prim > ##>)


(define-prim (##<= . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a <= b) })(g_scm2host(@1@));"
   (##list->vector xs)))
 (define-prim <= ##<=)
(define-prim (##>= . xs)
 (##inline-host-expression
   "((xs) => { return xs.reduce((a, b) => a >= b) })(g_scm2host(@1@));"
   (##list->vector xs)))
 (define-prim >= ##>=)

(define-prim (##expt x y)
  (##inline-host-expression "( @1@ ** @2@ )" x y))

(define-prim (expt x y)
  (macro-force-vars (x y)
    (##expt x y)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; abs
(define-prim (##abs x)
 (##inline-host-expression "Math.abs(@1@);" x))

(define-prim (abs x)
  (macro-force-vars (x)
    (##abs x)))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; gcd, lcm

(define-prim (##gcd . xs)
  (##inline-host-expression
"((arr) => {
  var i, y,
      n = arr.length,
      x = Math.abs(arr[0]);

  for (i = 1; i < n; i++) {
    y = Math.abs(arr[i]);

    while (x && y) {
      (x > y) ? x %= y : y %= x;
    }
    x += y;
  }
  return x;
})(g_scm2host(@1@))" (##list->vector xs)))

(define-prim gcd ##gcd)

(define-prim (##lcm . xs)
  (##inline-host-expression "((A) =>
{
    var n = A.length, a = Math.abs(A[0]);
    for (var i = 1; i < n; i++)
     { var b = Math.abs(A[i]), c = a;
       while (a && b){ a > b ? a %= b : b %= a; }
       a = Math.abs(c*A[i])/(a+b);
     }
    return a;
})(g_scm2host(@1@));" (##list->vector xs)))

(define-prim lcm ##lcm)

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(define-prim (##floor x)

  (define (type-error)
    (##fail-check-finite-real 1 floor x))

  (macro-number-dispatch x (type-error)
    x
    x
    (let ((num (macro-ratnum-numerator x))
          (den (macro-ratnum-denominator x)))
      (if (##negative? num)
          (##quotient (##- num (##- den 1)) den)
          (##quotient num den)))
    (if (##flfinite? x)
        (##flfloor x)
        (type-error))
    (if (macro-cpxnum-real? x)
        (##floor (macro-cpxnum-real x))
        (type-error))))

(define-prim (floor x)
  (macro-force-vars (x)
    (##floor x)))

(define-prim (##ceiling x)

  (define (type-error)
    (##fail-check-finite-real 1 ceiling x))

  (macro-number-dispatch x (type-error)
    x
    x
    (let ((num (macro-ratnum-numerator x))
          (den (macro-ratnum-denominator x)))
      (if (##negative? num)
          (##quotient num den)
          (##quotient (##+ num (##- den 1)) den)))
    (if (##flfinite? x)
        (##flceiling x)
        (type-error))
    (if (macro-cpxnum-real? x)
        (##ceiling (macro-cpxnum-real x))
        (type-error))))

(define-prim (ceiling x)
  (macro-force-vars (x)
    (##ceiling x)))


;; ;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(define-prim (##string->number str #!optional (rad 10))
  (if (= rad 10)
    (##inline-host-expression "Number(g_scm2host(@1@))" str)
    (##inline-host-expression "parseInt(g_scm2host(@1@), @2@)" str rad)))

(define-prim string->number ##string->number)

;;;----------------------------------------------------------------------------

;; UTF-8 encoding and decoding

(implement-library-type-invalid-utf8-encoding-exception)

(define-prim (##raise-invalid-utf8-encoding-exception proc . args)
  (##extract-procedure-and-arguments
   proc
   args
   #f
   #f
   #f
   (lambda (procedure arguments dummy1 dummy2 dummy3)
     (macro-raise
      (macro-make-invalid-utf8-encoding-exception
       procedure
       arguments)))))

(define-prim (##string->utf8-length
              str
              #!optional
              (start 0)
              (end (##string-length str)))

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (let loop ((i start)
             (len 0))
    (if (fx< i end)
        (let ((c (char->integer (string-ref str i))))
          (cond ((fx<= c #x7f)
                 ;; 1 byte encoding (ASCII)
                 (loop (fx+ i 1)
                       (fx+ len 1)))
                ((fx<= c #x7ff)
                 ;; 2 byte encoding
                 (loop (fx+ i 1)
                       (fx+ len 2)))
                ((fx<= c #xffff)
                 ;; 3 byte encoding
                 (loop (fx+ i 1)
                       (fx+ len 3)))
                (else
                 ;; 4 byte encoding
                 (loop (fx+ i 1)
                       (fx+ len 4)))))
        len)))

(define-prim (##string->utf8
              str
              #!optional
              (start 0)
              (end (##string-length str)))

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (let* ((len (##string->utf8-length str start end))
         (result (make-u8vector len)))
    (if (fx= len (fx- end start))
        (let loop1 ((i 0))
          (if (fx< i len)
              (begin
                (u8vector-set!
                 result
                 i
                 (char->integer
                  (string-ref str (fx+ start i))))
                (loop1 (fx+ i 1)))
              result))
        (let loop2 ((i start)
                    (j 0))
          (if (and (fx< i end)
                   (fx< j len)) ;; account for str mutation by other thread
              (let ((c (char->integer (string-ref str i))))
                (cond ((fx<= c #x7f)
                       ;; 1 byte encoding (ASCII)
                       (u8vector-set! result j c)
                       (loop2 (fx+ i 1)
                              (fx+ j 1)))
                      ((fx<= c #x7ff)
                       ;; 2 byte encoding
                       (u8vector-set!
                        result
                        j
                        (fx+ #xc0 (fxarithmetic-shift-right c 6)))
                       (u8vector-set!
                        result
                        (fx+ j 1)
                        (fx+ #x80 (fxand #x3f c)))
                       (loop2 (fx+ i 1)
                              (fx+ j 2)))
                      ((fx<= c #xffff)
                       ;; 3 byte encoding
                       (u8vector-set!
                        result
                        j
                        (fx+ #xe0 (fxarithmetic-shift-right c 12)))
                       (u8vector-set!
                        result
                        (fx+ j 1)
                        (fx+ #x80 (fxand #x3f (fxarithmetic-shift-right c 6))))
                       (u8vector-set!
                        result
                        (fx+ j 2)
                        (fx+ #x80 (fxand #x3f c)))
                       (loop2 (fx+ i 1)
                              (fx+ j 3)))
                      (else
                       ;; 4 byte encoding
                       (u8vector-set!
                        result
                        j
                        (fx+ #xf0 (fxarithmetic-shift-right c 18)))
                       (u8vector-set!
                        result
                        (fx+ j 1)
                        (fx+ #x80 (fxand #x3f (fxarithmetic-shift-right c 12))))
                       (u8vector-set!
                        result
                        (fx+ j 2)
                        (fx+ #x80 (fxand #x3f (fxarithmetic-shift-right c 6))))
                       (u8vector-set!
                        result
                        (fx+ j 3)
                        (fx+ #x80 (fxand #x3f c)))
                       (loop2 (fx+ i 1)
                              (fx+ j 4)))))
              result)))))

(define-prim (string->utf8
              str
              #!optional
              (start (macro-absent-obj))
              (end (macro-absent-obj)))

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
  (namespace ("" string->utf8)) ;; but not string->utf8 to ##string->utf8

  (macro-force-vars (str start end)
    (macro-check-string
      str
      1
      (string->utf8 str start end)
      (if (eq? start (macro-absent-obj))
          (##string->utf8 str)
          (macro-check-index-range-incl
            start
            2
            0
            (string-length str)
            (string->utf8 str start end)
            (if (eq? end (macro-absent-obj))
                (##string->utf8 str start)
                (macro-check-index-range-incl
                  end
                  3
                  start
                  (string-length str)
                  (string->utf8 str start end)
                  (##string->utf8 str start end))))))))

(define-prim (##utf8->string-length
              u8vect
              #!optional
              (start 0)
              (end (##u8vector-length u8vect)))

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (let loop ((i start)
             (len 0))
    (if (fx< i end)
        (let ((b0 (u8vector-ref u8vect i)))
          (cond ((fx< b0 #x80)
                 ;; 1 byte encoding (ASCII)
                 (loop (fx+ i 1)
                       (fx+ len 1)))
                ((fx< b0 #xe0)
                 ;; 2 byte encoding or invalid encoding
                 (loop (fx+ i 2)
                       (fx+ len 1)))
                ((fx< b0 #xf0)
                 ;; 3 byte encoding or invalid encoding
                 (loop (fx+ i 3)
                       (fx+ len 1)))
                (else
                 ;; 4 byte encoding or invalid encoding
                 (loop (fx+ i 4)
                       (fx+ len 1)))))
        (if (fx> i end)
            0 ;; invalid or truncated encoding
            len))))

(define-prim (##utf8->string
              u8vect
              #!optional
              (start 0)
              (end (##u8vector-length u8vect)))

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (define (invalid-utf8)
    (##raise-invalid-utf8-encoding-exception utf8->string u8vect start end))

  (let* ((len (##utf8->string-length u8vect start end))
         (result (make-string len)))
    (if (fx= len (fx- end start))
        (let loop1 ((i 0))
          (if (fx< i len)
              (begin
                (string-set!
                 result
                 i
                 (integer->char
                  (u8vector-ref u8vect (fx+ start i))))
                (loop1 (fx+ i 1)))
              result))
        (let loop2 ((i start)
                    (j 0))
          (if (fx< i end)
              (if (fx< j len) ;; account for u8vect mutation by other thread
                  (let ((b0 (u8vector-ref u8vect i)))
                    (cond ((fx< b0 #x80)
                           ;; 1 byte encoding (ASCII)
                           (string-set!
                            result
                            j
                            (integer->char b0))
                           (loop2 (fx+ i 1)
                                  (fx+ j 1)))
                          ((fx< b0 #xc2)
                           (invalid-utf8))
                          ((fx< b0 #xe0)
                           ;; 2 byte encoding
                           (let* ((b1 (u8vector-ref
                                       u8vect
                                       (fx+ i 1)))
                                  (n (fx+
                                      (fxarithmetic-shift-left
                                       (fxand b0 #x1f)
                                       6)
                                      (fxand b1 #x3f))))
                             (if (and (fx= (fxand b1 #xc0)
                                           #x80)
                                      (fx>= n #x80))
                                 (begin
                                   (string-set!
                                    result
                                    j
                                    (integer->char n))
                                   (loop2 (fx+ i 2)
                                          (fx+ j 1)))
                                 (invalid-utf8))))
                          ((fx< b0 #xf0)
                           ;; 3 byte encoding
                           (let* ((b1 (u8vector-ref
                                       u8vect
                                       (fx+ i 1)))
                                  (b2 (u8vector-ref
                                       u8vect
                                       (fx+ i 2)))
                                  (n (fx+
                                      (fxarithmetic-shift-left
                                       (fxand b0 #x0f)
                                       12)
                                      (fxarithmetic-shift-left
                                       (fxand b1 #x3f)
                                       6)
                                      (fxand b2 #x3f))))
                             (if (and (fx= (fxand (fxior b1
                                                         b2)
                                                  #xc0)
                                           #x80)
                                      (fx>= n #x800)
                                      (not
                                       (and (fx>= n #xd800)
                                            (fx<= n #xdfff))))
                                 (begin
                                   (string-set!
                                    result
                                    j
                                    (integer->char n))
                                   (loop2 (fx+ i 3)
                                          (fx+ j 1)))
                                 (invalid-utf8))))
                          ((fx< b0 #xf5)
                           ;; 4 byte encoding
                           (let* ((b1 (u8vector-ref
                                       u8vect
                                       (fx+ i 1)))
                                  (b2 (u8vector-ref
                                       u8vect
                                       (fx+ i 2)))
                                  (b3 (u8vector-ref
                                       u8vect
                                       (fx+ i 3)))
                                  (n (fx+
                                      (fxarithmetic-shift-left
                                       (fxand b0 #x07)
                                       18)
                                      (fxarithmetic-shift-left
                                       (fxand b1 #x3f)
                                       12)
                                      (fxarithmetic-shift-left
                                       (fxand b2 #x3f)
                                       6)
                                      (fxand b3 #x3f))))
                             (if (and (fx= (fxand (fxior b1
                                                         b2
                                                         b3)
                                                  #xc0)
                                           #x80)
                                      (fx>= n #x10000)
                                      (fx<= n #x10ffff))
                                 (begin
                                   (string-set!
                                    result
                                    j
                                    (integer->char n))
                                   (loop2 (fx+ i 4)
                                          (fx+ j 1)))
                                 (invalid-utf8))))
                          (else
                           (invalid-utf8))))
                  (invalid-utf8))
              (if (or (fx> i end)
                      (fx< j len))
                  (invalid-utf8)
                  result))))))

(define-prim (utf8->string
              u8vect
              #!optional
              (start (macro-absent-obj))
              (end (macro-absent-obj)))

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
  (namespace ("" utf8->string)) ;; but not utf8->string to ##utf8->string

  (macro-force-vars (u8vect start end)
    (macro-check-u8vector
      u8vect
      1
      (utf8->string u8vect start end)
      (if (eq? start (macro-absent-obj))
          (##utf8->string u8vect)
          (macro-check-index-range-incl
            start
            2
            0
            (u8vector-length u8vect)
            (utf8->string u8vect start end)
            (if (eq? end (macro-absent-obj))
                (##utf8->string u8vect start)
                (macro-check-index-range-incl
                  end
                  3
                  start
                  (u8vector-length u8vect)
                  (utf8->string u8vect start end)
                  (##utf8->string u8vect start end))))))))

;;;============================================================================
