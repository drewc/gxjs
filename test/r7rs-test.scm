(define (test> name i pred j)
  (let ((res (pred i j)))
    (if res
      (##inline-host-statement
       "try {
          console.log('Success!', g_scm2host(@1@), g_scm2host(@2@));
        } catch {
         console.log('Made it!', g_scm2host(@1@), (@2@));
       };"
       name i)
      (##inline-host-statement
       " try {
            console.error('failed :(', g_scm2host(@1@), g_scm2host(@2@), 'not predicated', g_scm2host(@3@), (@3@), (@2@));
        } catch {
          console.error('total fail', g_scm2host(@1@), (@2@), (@3@))
        }"
       name i j))))

(define (test-r7rs)
  (test> "1 + 1 + 1 using + =" (+ 1 1 1) = 3)
  (test> "1 + 1 + 1.5 using + =" (+ 1 1 1.5) = 3.5)
  (test> "2 x 2 x 2 using * =" (* 2 2 2) = 8)
  (test> "1 / 2 using / =" (/ 1 2) = 0.5)
  (test> "2-1.5-.5 using - =" (- 2 1.5 .5) = 0)
  (test> "0 = 0 " 0 = 0)
  (test> "2 < 3 < 4 using < eq? #t " (< 2 3 4) eq? #t)
  (test> "2 > 1 > 0 using > eq? #t " (> 2 1 0) eq? #t)
  (test> "2 <= 3 <= 3 using <= eq? #t " (<= 2 3 3) eq? #t)
  (test> "3 >= 3 >= 1 using >= eq? #t " (>= 3 3 1) eq? #t)

  (test> "abs(1.5)" (abs -1.5) = 1.5)
  (test> "and is auto? and #t #t 42" (and #t #t 42) = 42)
  (test> "append works" (append '(1) '(4) '(2)) equal? '(1 4 2))
  (test> "Assoc 1 ((1 . 42) (6 . 2))'"
         (assoc 1 '((1 . 42) (6 . 2))) equal? '(1 . 42))
  (test> "Assq 'one ((one . 42) (6 . 2))'"
         (assq 'one '((one . 42) (6 . 2))) equal? '(one . 42))
  (test> "Assv 'one ((one . 42) (6 . 2))'"
         (assv 'one '((one . 42) (6 . 2))) equal? '(one . 42))
  (test> "begin works" (begin '(1) '(4) 42) = 42)
  (test> "boolean=?" (boolean=? #t #t #t) eq? #t)
  (test> "boolean=? fail" (boolean=? #t #f #t) eq? #f)
  (test> "boolean? #t" (boolean? #t) eq? #t)
  (test> "boolean? #f" (boolean? #f) eq? #t)
  (test> "boolean? 'foo" (boolean? 'foo) eq? #f)
  (test> "bytevector" (let* ((b (bytevector 4))
                             (v (bytevector 2))
                             (bv (bytevector-append b v))
                             (a (bytevector-copy b))
                             (u (bytevector 4 0))
                             (_ (bytevector-copy! u 1 (bytevector 4 2 0) 1 2))
                             (l (bytevector-length u))
                             (four (bytevector-u8-ref bv 0))
                             (_ (bytevector-u8-set! v 0 42)))

                        (list
                         (bytevector? bv)
                         (equal? bv (bytevector 4 2))
                         (equal? v (bytevector 42))
                         (equal? a b)
                         (equal? u bv)
                         (= 2 l)
                         (= 4 four)))
         equal? '(#t #t #t #t #t #t #t))


  (test> "caar ((42))" (caar '((42))) = 42)
  (test> "cadr (41 42)" (cadr '(41 42)) = 42)
  #;(##inline-host-statement "console.error(@1@)" (call-with-values (lambda (a b c) a) (values 1 2 3)))

  (test> "car (42)" (car '(42)) = 42)
  (test> "case?" (case 'foo
                   ((foo) 42)
                   (else 0)) = 42)
  (test> "cdar ((1 . 42))" (cdar '((1 . 42))) = 42)
  (test> "cddr (0 1 . 42)" (cddr '(0 1 . 42)) = 42)
  (test> "cdr (1 . 42)" (cdr '(1 . 42)) = 42)
  (test> "ceiling 41.42" (ceiling 41.42) = 42)
  (test> "char->integer #\\*" (char->integer #\*) = 42)
   ;; char-ready?
  (test> "char<=? #\\a #\\b #\\b #\\c" (char<=? #\a #\b #\b #\c) eq? #t)
  (test> "char<? #\\a #\\b #\\c" (char<? #\a #\b  #\c) eq? #t)
  (test> "char=? #\\a #\\b => #f" (char=? #\a #\b) eq? #f)
  (test> "char=? #\\a #\\a => #t" (char=? #\a #\a) eq? #t)
  (test> "char>=? #\\a #\\b #\\c => #f" (char>=? #\a #\b  #\c) eq? #f)
  (test> "char>? #\\a #\\b #\\c => #f" (char>? #\a #\b  #\c) eq? #f)
  (test> "char? #\\a" (char? #\a) eq? #t)
  (test> "char? 42 => #f" (char? 42) eq? #f)

  (test> "complex? 42" (complex? 42) eq? #t)

  (test> "cond" (cond ((= 42 42.0) 42) (else 0))= 42)
  (test> "cond else" (cond ((= 42 42.1) 42) (else 0)) equal? 0)
  (test> "cond-expand" (cond-expand (foo 41) (else 42)) = 42)

  (test> "cons" (cons 4 2) equal? '(4 . 2))
  ;;(##inline-host-statement "console.error(g_scm2host(@1@))" '(("make" . "me") ("an" . "object")))

  (test> "define" ((lambda () (define n 42) (define (en) n) (en))) = 42)
  ;; Works but needs structs
  (test> "define-record-type"
         ((lambda ()
            (define-record-type <pare>
              (kons x y)
              pare?
              (x kar set-kar!)
              (y kdr)) (kar (kons 42 0)))
          )
         = 42)
  (test> "define-syntax"
         ((lambda ()
            (define-syntax %when
              (syntax-rules ()
                ((%when test result1 result2 ...)
                 (if test
                   (begin result1 result2 ...)))))

            (%when #t 42)))
         = 42)

  ;; (test> "define-values" ((lambda () (define-values (b a) (values 42 42)) a)) eq? 42)

  (test> "denominator 1/2" (denominator 1/2) = 2)

  (test> "do" (do ((vec (make-vector 5))
                   (i 0 (+ i 1)))
                  ((= i 5) vec)
                (vector-set! vec i i))
         equal? #(0 1 2 3 4))

  ;; (test> "dynamic-wind" (dynamic-wind (lambda () 41) (lambda () 42) (lambda () 43)) = 42)
  (test> "eof object" (eof-object? (eof-object)) eq? #t)
  (test> "eq? 12 12.0 => #f" (eq? 12 12.0) eq? #f)
  (test> "eq? 'this 'this  => #t" (eq? 'this 'this) eq? #t)
  (test> "eqv? 12 12.0 => #f" (eqv? 12 12.0) eq? #f)
  (test> "eqv? 12 12 => #t" (eqv? 12 12) eq? #t)
  (test> "equal? 12 12.0 => #f" (equal? 12 12.0) eq? #f)
  (test> "equal? \"12\" \"12\" => #t" (equal? "12" "12") eq? #t)
  ;; (test> "error" (error "Does error work?") eq? #t)
  (test> "even? 12 => #t" (even? 12) eq? #t)
  ;; (test> "(exact 0.5) => 1/2)" (exact 0.5) equal? 1/2)
  (test> "(exact-integer? 0.5) => #f)" (exact-integer? 0.5) eq? #f)
  ;; (test> "(exact-integer-sqrt r5) => #t)" (exact-integer-sqrt 5) eq? #t)
  (test> "(exact? 5) => #t)" (exact? 5) eq? #t)
  (test> "(exact? 42.6666) => #f)" (exact? 42.666) eq? #f)
  (test> "(exact? 1/3) => #t)" (exact? 1/3) eq? #t)
  (test> "(expt 2 2) => 4)" (expt 2 2) = 4)
  ;; (test> "(list? (features)) => #t)" (list? (features)) eq? #t)
  (test> "(floor 3/2) => 1)" (floor 3/2) = 1)
  (test> "(floor-quotient 5 2) => 2)" (floor-quotient 5 2) = 2)
  (test> "(floor-remainder 5 2) => 1)" (floor-remainder 5 2) = 1)
  (test> "for-each" ((lambda ()
                       (define n 0)
                       (for-each (lambda (k) (set! n k))
                                 '(1 2 3 42))
                       n)) = 42)
  (test> "(gcd 5 2) => 1)" (##gcd 5 2) = 1)
  #;(test> "guard" (guard (condition
                         ((assq ’a condition) => cdr)
                         ((assq ’b condition)))
                   (raise (list (cons ’a 42))))
         = 42)

  (test> "integer->char" (integer->char 42) equal? #\*)
  (test> "integer? 42" (integer? 42) eq? #t)
  (test> "integer? 42.42" (integer? 42.42) eq? #f)

  ;(raise (list (cons a 42)))

  (test> "lcm 2 7" (lcm 2 7) eq? 14)
  (test> "let" (let ((a 20) (b 20) (c 2)) (+ a b c)) = 42)
  (test> "let*" (let* ((a 20) (b (+ a 20)) (c (+ b 2))) c) = 42)
  #;(test> "let*-values" (let*-values (((a b) (values 20 1))
                                     ((x y) (values a b)))
                         (+ a b x y)) = 42)

  #;(test> "let-syntax" (let ((x ’outer))
                        (let-syntax ((m (syntax-rules () ((m) x))))
                          (let ((x ’inner))
                            (m)))) eq? 'outer)

  (test> "letrec" (letrec ((even?
                            (lambda (n)
                              (if (zero? n)
                                #t
                                (odd? (- n 1)))))
                           (odd?
                            (lambda (n)
                              (if (zero? n)
                                #f
                                (even? (- n 1))))))
                    (even? 88))
         eq? #t)

  (test> "letrec*" (let ((x 5))
                     (letrec* ((foo (lambda (y) (bar x y)))
                               (bar (lambda (a b) (+ (* a b) a))))
                       (foo (+ x 3)))) = 45)

  (test> "list" (list 4 2) equal? '(4 2))
  (test> "list->string" (list->string '(#\4 #\2)) equal? "42")
  (test> "list->vector" (list->vector '(4 2)) equal? (vector 4 2))
  (test> "list-copy" (list-copy (list 4 2)) equal? '(4 2))
  (test> "list-ref" (list-ref (list 4 2 42) 2) equal? 42)
  (test> "list-set!" (let ((lst (list 4 2 3)))
                      (list-set! lst 2 42)
                      (list-ref lst 2)) equal? 42)
  (test> "list-tail" (list-tail '(1 2 3) 2) equal? '(3))
  (test> "list?" (list (list? '(1 2 3)) (list? '()) (list? 2)) equal? '(#t #t #f))

  (test> "make-bytevector" (make-bytevector 4 2) equal? (make-bytevector 4 2))
  (test> "make-list" (make-list 4 2) equal? '(2 2 2 2))
  (test> "make-parameter" ((lambda () (define p (make-parameter 42))
                             (p))) = 42)

  (test> "make-string" (make-string 4 #\2) equal? "2222")
  (test> "make-vector" (make-vector 4 2) equal? #(2 2 2 2))
  (test> "map" (map (lambda (n) (+ 1 n)) '(3 1)) equal? '(4 2))
  (test> "max" (max 41 42 39 38) = 42)
  (test> "member" (car (member "1" '("1" 3 4))) equal? "1")
  (test> "memq" (car (memq 'fourtwo '("1" 3 fourtwo 4))) equal? 'fourtwo)
  (test> "memv" (car (memv 4 '("1" 3 fourtwo 4))) equal? 4)
  (test> "min" (min 141 42 339 358) = 42)
  (test> "modulo" (modulo 4 3) = 1)
  (test> "negative?" (negative? -3) eq? #t)
  (test> "not" (not #f) eq? #t)
  (test> "null?" (null? '()) eq? #t)
  (test> "number->string" (number->string 42) equal? "42")
  (test> "numerator" (numerator 1/2) = 1)
  (test> "odd? 42" (odd? 42) eq? #f)
  (test> "odd? 41" (odd? 41) eq? #t)
  (test> "or 42" (or (odd? 40) #f 42) = 42)
  (test> "pair?" (list (pair? 1) (pair? '(1))) equal? '(#f #t))
  (test> "parameterize yay!" ((lambda () (define p (make-parameter 41))
                            (parameterize ((p 42)) (p)))) = 42)

  (test> "positive?" (list (positive? -1) (positive? 42)) equal? '(#f #t))
  (test> "procedure?" (list (procedure? -1) (procedure? list)) equal? '(#f #t))
  (test> "quasiquote" (quasiquote (life the universe and everyhing))
         equal? '(life the universe and everyhing))
  (test> "quote" (quote (life the universe and everyhing))
         equal? '(life the universe and everyhing))

  (test> "quotient" (quotient 84 2) = 42)
  ;d; No raise yet
  (test> "rational?" (list (rational? 1) (rational? 1/2) (rational? -inf.0)) equal? '(#t #t #f))
  #;(test> "rationalize" (rationalize .3 1/10) = #t )
  (test> "real?" (list (real? 1) (real? 1/2)) equal? '(#t #t))

  (test> "remainder" (remainder 42 9) = 6)
  (test> "reverse" (reverse '(2 4)) equal? '(4 2))
  (test> "round" (round 41.7) = 42)

  (test> "set!" (let ((a 1)) (set! a 42) a) = 42)
  (test> "set-car!" (let ((a (list 1 2))) (set-car! a 42) a) equal? '(42 2))
  (test> "set-cdr!" (let ((a (list 1 2))) (set-cdr! a 42) a) equal? '(1 . 42))

  (test> "square" (square 2) = 4)

  (test> "string" (string #\*) equal? "*")
  (test> "string->list" (string->list "42") equal? (list #\4 #\2))
  (test> "string->number" (string->number "42") = 42)
  (test> "string->symbol" (string->symbol "42") eq? '|42|)
  (test> "string->utf8" (bytevector-u8-ref (string->utf8 "*") 0) eq? 42)
  (test> "string->vector" (string->vector "42") equal? (vector #\4 #\2))
  (test> "string-append" (string-append "4" "2") equal? "42")
  (test> "string-copy" (string-copy "42") equal? "42")
  (test> "string-copy!" ((lambda ()
                           (define a "42345")
                           (define b (string-copy "abcde"))
                           (string-copy! b 1 a 0 2)
                           b))
         equal? "a42de")
  (test> "string-fill!" (let ((str "0242"))
                          (string-fill! str #\4 0 1) str)
         equal? "4242")
  (test> "string-for-each" (let ((lst '()))
                             (string-for-each (lambda (c) (set! lst (cons c lst)))
                                              "24")
                             lst)
         equal? (list #\4 #\2))
  (test> "string-length" (string-length (make-string 42 #\0)) = 42)
  (test> "string-ref" (string-ref "42" 0) eqv? #\4)
  (test> "string-set!" ((lambda () (define str "02") (string-set! str 0 #\4) str))
         equal? "42")
  (test> "string<=?" (list (string<=? "42" "42") (string<=? "41" "42") (string<=? "43" "42"))
         equal? '(#t #t #f))
  (test> "string<?" (list (string<? "42" "42") (string<? "41" "42") (string<? "43" "42"))
         equal? '(#f #t #f))
  (test> "string>?" (list (string>? "42" "42") (string>? "41" "42") (string>? "43" "42"))
         equal? '(#f #f #t))

  (test> "string>=?" (list (string>=? "42" "42") (string>=? "41" "42") (string>=? "43" "42"))
         equal? '(#t #f #t))
  (test> "substring" (substring "0420" 1 3) equal? "42")

  (test> "symbol->string" (symbol->string '|42|) equal? "42")
  (test> "symbol=?" (list (symbol=? '|42|'|42| '|42|)
                          (symbol=? '|42|'|42| 'nope))
         equal? '(#t #f))
  (test> "symbol?" (list (symbol?  '|42|)
                          (symbol? 42))
         equal? '(#t #f))

  (test> "syntax-rules" ((lambda ()
                           (define-syntax be-like-begin
                             (syntax-rules ()
                               ((be-like-begin name)
                                (define-syntax name
                                  (syntax-rules ()
                                    ((name expr (... ...))
                                     (begin expr (... ...))))))))
                           (be-like-begin foo)
                           (foo 12 34 42)
                           )) = 42)

  (test> "truncate" (list (truncate -4.3) (truncate 3.5)) equal? '(-4.0 3.0))
  (test> "truncate-quotient" (list (truncate-quotient 4 2) (truncate-quotient 7 5)) equal? '(2 1))
  (test> "truncate-remainder" (list (truncate-remainder 4 2) (truncate-remainder 7 5)) equal? '(0 2))

  #;(test> "truncate/" ((lambda () 1
                        (let-values (((q r) (truncate/ 7 5))
                          (list q r))
                        ))) equal? '( 1 2))

  (test> "Unless" (unless #f 42) = 42)
  (test> "unquote and splicing" (quasiquote ((unquote 4) (unquote-splicing '(2))))
         equal? '(4 2))
  (test> "utf8->string" (utf8->string (bytevector 52 50)) equal? "42")

  (test> "values" (##values->list (values 4 2)) equal? '(4 2))

  (test> "vector"
         (let* ((v (vector 42 4 2))
               (lst (vector->list v))
               (sv (vector #\4 #\2))
               (str (vector->string sv))
               (lststr (vector-append v sv))
               (vc (vector-copy lststr 3 4))
               (u (vector 4 0))
               (_ (vector-copy! u 1 (vector 4 2 0) 1 2))
               (fv (vector 0 4 0))
               (_ (vector-fill! fv 2 2 3))
               (vl (vector-length (vector 0 1 2)))
               (vm (vector-map (lambda (i) (+ 1 i)) (vector 3 1)))
               (vr (vector-ref (vector 0 42 1) 1))
               (vs (let ((v (vector 1 2)))
                     (vector-set! v 0 4)
                     v))
               (v? (vector? #(1 2 3))))
           (list v
                 lst
                 str
                 lststr
                 vc
                 u
                 fv
                 vl
                 vm
                vr
                vs
                v?))
         equal?
         (list #(42 4 2)
               '(42 4 2)
               "42"
               #(42 4 2 #\4 #\2)
               #(#\4)
               #(4 2)
               #(0 4 2)
               3
               #(4 2)
               42
                #(4 2)
                #t))

  (test> "when" (when #t 42) = 42)

  (test> "with-execption-handler" (with-exception-handler
                                  (lambda (e) (write e) 5)
                                  (lambda () (+ 1 (* 2 3) 35)))
        = 42)

  (test> "zero?" (list (zero? 0) (zero? 42)) equal? '(#t #f))


  )

;; (##inline-host-statement "console.log(@1@)" (error "here"))
;; (##inline-host-statement "alert('r7rs')")
(test-r7rs)
