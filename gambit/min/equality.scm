;;;----------------------------------------------------------------------------

;;; Object equality.

;;; imports:
;;; from _kernel.scm
;;;    (##type-fields ...)
;;;    (##type-flags ...)
;;;    (##type-id ...)
;;;    (##type-super ...)
;;; from _num.scm
;;;    (##exact-int.= ...)
;;;    (##ratnum.= ...)
;;; from _std.scm
;;;    (##f32vector-equal? ...)
;;;    (##f64vector-equal? ...)
;;;    (##s16vector-equal? ...)
;;;    (##s32vector-equal? ...)
;;;    (##s64vector-equal? ...)
;;;    (##s8vector-equal? ...)
;;;    (##string-equal? ...)
;;;    (##u16vector-equal? ...)
;;;    (##u32vector-equal? ...)
;;;    (##u64vector-equal? ...)
;;;    (##u8vector-equal? ...)
;;;    (##vector-equal? ...)

;;; exports:
;;;    (##case-memv ...)
;;;    (##eq? ...)
;;;    (##equal? ...)
;;;    (##eqv? ...)
;;;    (eq? ...)
;;;    (equal? ...)
;;;    (eqv? ...)

;;;----------------------------------------------------------------------------

;;; Support for the case special form.

;;; The compiler frontend converts
;;;
;;;   (case x ((a b) c) ...)
;;;
;;; to
;;;
;;;   (if (##case-memv x '(a b)) c ...)

(define-prim (##case-memv obj lst)
  (macro-force-vars (obj)
    (let loop ((x lst))
      (if (##pair? x)
          (if (let () (##declare (generic)) (##eqv? obj (##car x)))
              x
              (loop (##cdr x)))
          #f))))

;;;----------------------------------------------------------------------------

(##define-macro (macro-numeqv?-otherwise obj1 obj2 true false otherwise)
  `(macro-number-dispatch ,obj1 ,otherwise
     (if (##fixnum? ,obj2) ;; obj1 = fixnum
         (if (##fx= ,obj1 ,obj2)
             ,true
             ,false)
         ,false)
     (if (##bignum? ,obj2) ;; obj1 = bignum
         (if (##exact-int.= ,obj1 ,obj2)
             ,true
             ,false)
         ,false)
     (if (##ratnum? ,obj2) ;; obj1 = ratnum
         (if (##ratnum.= ,obj1 ,obj2)
             ,true
             ,false)
         ,false)
     (if (##flonum? ,obj2) ;; obj1 = flonum
         (if (##fleqv? ,obj1 ,obj2)
             ,true
             ,false)
         ,false)
     (if (##cpxnum? ,obj2) ;; obj1 = cpxnum
         (if (and (##eqv? (macro-cpxnum-real ,obj1) (macro-cpxnum-real ,obj2))
                  (##eqv? (macro-cpxnum-imag ,obj1) (macro-cpxnum-imag ,obj2)))
             ,true
             ,false)
         ,false)))

(define-prim (##eqv? obj1 obj2)
  (or (##eq? obj1 obj2)
      (macro-numeqv?-otherwise
       obj1
       obj2
       #t
       #f
       #f)))

(define-prim (eqv? obj1 obj2)
  (macro-force-vars (obj1 obj2)
    (let ()
      (##declare (generic)) ;; avoid fixnum specific ##eqv?
      (##eqv? obj1 obj2))))

(define-prim (##eq? obj1 obj2))

(define-prim (eq? obj1 obj2)
  (macro-force-vars (obj1 obj2)
    (##eq? obj1 obj2)))

(define-prim-nary-bool (##symbol=? x y)
  #t
  #t
  (##eq? x y)
  macro-no-force
  macro-no-check)

(define-prim-nary-bool (symbol=? x y)
  #t
  (if (##symbol? x) #t '(1))
  (##eq? x y)
  macro-force-vars
  macro-check-symbol
  (##pair? ##fail-check-symbol))

(define-prim-nary-bool (##boolean=? x y)
  #t
  #t
  (##eq? x y)
  macro-no-force
  macro-no-check)

(define-prim-nary-bool (boolean=? x y)
  #t
  (if (##boolean? x) #t '(1))
  (##eq? x y)
  macro-force-vars
  macro-check-boolean
  (##pair? ##fail-check-boolean))

;;-----------------------------------------------------------------------------

(##define-macro (macro-define-equal-objs?
                 equal-objs?
                 params
                 custom-recursion-handler
                 .
                 local-defines)

  `(define (,equal-objs? obj1 obj2 ,@params)

     ,@local-defines

     ,@(if custom-recursion-handler
           `()
           `((define (table-equal obj1 obj2 ,@params)
               (conj (gc-hash-table-equal (macro-table-gcht obj1)
                                          obj2
                                          ,@params)
                     (if (macro-table-test obj1)
                         (gc-hash-table-equal (macro-table-hash obj1)
                                              obj2
                                              ,@params)
                         (true))))

             (define (gc-hash-table-equal ht1 table2 ,@params)
               (##declare (not interrupts-enabled))
               (if (##gc-hash-table? ht1)
                   (let loop ((i (macro-gc-hash-table-key0))
                              ,@(map (lambda (p) `(,p ,p))
                                     params))
                     (if (##fx< i (##vector-length ht1))
                         (let ((key1 (##vector-ref ht1 i)))
                           (if (or (##eq? key1 (macro-unused-obj))
                                   (##eq? key1 (macro-deleted-obj)))
                               (let ()
                                 (##declare (interrupts-enabled))
                                 (loop (##fx+ i 2)
                                       ,@params))
                               (let* ((val1
                                       (##vector-ref ht1 (##fx+ i 1)))
                                      (val2
                                       (##table-ref table2
                                                    key1
                                                    (macro-unused-obj))))
                                 (conj (,equal-objs?
                                        val1
                                        val2
                                        ,@params)
                                       (let ()
                                         (##declare (interrupts-enabled))
                                         (loop (##fx+ i 2)
                                               ,@params))))))
                         (true)))))

             (define (structure-equal obj1 obj2 type len ,@params)
               (if (##not type) ;; have we reached root of inheritance chain?
                   (true)
                   (let ((fields (##type-fields type)))
                     (let loop ((i*3 (##fx- (##vector-length fields) 3))
                                (len len)
                                ,@(map (lambda (p) `(,p ,p))
                                       params))
                       (if (##fx< i*3 0) ;; time to check inherited fields?
                           (structure-equal obj1
                                            obj2
                                            (##type-super type)
                                            len
                                            ,@params)
                           (let ((field-attributes
                                  (##vector-ref fields (##fx+ i*3 1)))
                                 (len-1
                                  (##fx- len 1)))
                             (if (##not (##fx= ;; equality-skip flag set?
                                         (##fxand field-attributes 4)
                                         0))
                                 (loop (##fx- i*3 3) ;; don't check this field
                                       len-1
                                       ,@params)
                                 (conj (,equal-objs? (##unchecked-structure-ref
                                                      obj1
                                                      len-1
                                                      type
                                                      #f)
                                                     (##unchecked-structure-ref
                                                      obj2
                                                      len-1
                                                      type
                                                      #f)
                                                     ,@params)
                                       (loop (##fx- i*3 3)
                                             len-1
                                             ,@params)))))))))))

     (macro-force-vars (obj1 obj2)
       (if (##eq? obj1 obj2)
           (begin
             (profile! 0)
             (true))
           (cond ((##pair? obj1)
                  (profile! 1)
                  (if (##not (##pair? obj2))
                      (false)
                      ,(if custom-recursion-handler
                           `(,custom-recursion-handler obj1 obj2 ,@params)
                           `(recursion
                             obj1
                             obj2
                             (conj (,equal-objs? (##car obj1)
                                                 (##car obj2)
                                                 ,@params)
                                   (,equal-objs? (##cdr obj1)
                                                 (##cdr obj2)
                                                 ,@params))))))
                 ((##vector? obj1)
                  (profile! 2)
                  (if (and (##vector? obj2)
                           (##vector-equal? obj1 obj2))
                    (true)
                    (false)))
                 #;((##vector? obj1)
                  (profile! 2)
                  (if (##not (##vector? obj2))
                      (false)
                      (let ((len (##vector-length obj1)))
                        (if (##not (##fx= len (##vector-length obj2)))
                            (false)
                            ,(if custom-recursion-handler
                                 `(,custom-recursion-handler obj1 obj2 ,@params)
                                 `(recursion
                                   obj1
                                   obj2
                                   (let loop ((i (##fx- len 1))
                                              ,@(map (lambda (p) `(,p ,p))
                                                     params))
                                     (if (##fx< i 0)
                                         (true)
                                         (conj (,equal-objs?
                                                (##vector-ref obj1 i)
                                                (##vector-ref obj2 i)
                                                ,@params)
                                               (loop (##fx- i 1)
                                                     ,@params))))))))))
                 ((##fixnum? obj1)
                  (profile! 3)
                  (if (and (##fixnum? obj2)
                           (##fx= obj1 obj2))
                      (true)
                      (false)))
                 ((##bignum? obj1)
                  (profile! 4)
                  (if (and (##bignum? obj2)
                           (##exact-int.= obj1 obj2))
                      (true)
                      (false)))
                 ((##ratnum? obj1)
                  (profile! 5)
                  (if (and (##ratnum? obj2)
                           (##ratnum.= obj1 obj2))
                      (true)
                      (false)))
                 ((##flonum? obj1)
                  (profile! 6)
                  (if (and (##flonum? obj2)
                           (##fleqv? obj1 obj2))
                      (true)
                      (false)))
                 ((##cpxnum? obj1)
                  (profile! 7)
                  (if (and (##cpxnum? obj2)
                           (##eqv? (macro-cpxnum-real obj1)
                                   (macro-cpxnum-real obj2))
                           (##eqv? (macro-cpxnum-imag obj1)
                                   (macro-cpxnum-imag obj2)))
                      (true)
                      (false)))
                 ((macro-table? obj1)
                  (profile! 8)
                  (if (##not (and (macro-table? obj2)
                                  (##fx= (macro-table-flags obj1)
                                         (macro-table-flags obj2))
                                  (##eq? (macro-table-test obj1)
                                         (macro-table-test obj2))
                                  (if (macro-table-test obj1)
                                      (##eq? (macro-table-hash obj1)
                                             (macro-table-hash obj2))
                                      #t)
                                  (##fx= (##table-length obj1)
                                         (##table-length obj2))))
                      (false)
                      ,(if custom-recursion-handler
                           `(,custom-recursion-handler
                             obj1
                             obj2
                             ,@params)
                           `(recursion
                             obj1
                             obj2
                             (table-equal
                              obj1
                              obj2
                              ,@params)))))
                 ((##structure? obj1)
                  (profile! 9)
                  (if (##not (##structure? obj2))
                      (false)
                      (let ((type (##structure-type obj1)))
                        (if (##not (##eq? (##type-id type)
                                          (##type-id
                                           (##structure-type obj2))))
                            (false)
                            (let ((len (##structure-length obj1)))
                              (if (##not
                                   (and (##fx=
                                         len
                                         (##structure-length obj2))
                                        (##fx= ;; not opaque?
                                         (##fxand
                                          (##type-flags type)
                                          1)
                                         0)))
                                  (false)
                                  ,(if custom-recursion-handler
                                       `(,custom-recursion-handler
                                         obj1
                                         obj2
                                         ,@params)
                                       `(recursion
                                         obj1
                                         obj2
                                         (structure-equal
                                          obj1
                                          obj2
                                          type
                                          len
                                          ,@params)))))))))
                 ((##box? obj1)
                  (profile! 10)
                  (if (##not (##box? obj2))
                      (false)
                      ,(if custom-recursion-handler
                           `(,custom-recursion-handler
                             obj1
                             obj2
                             ,@params)
                           `(recursion
                             obj1
                             obj2
                             (,equal-objs?
                              (##unbox obj1)
                              (##unbox obj2)
                              ,@params)))))
                 ((##string? obj1)
                  (profile! 11)
                  (if (and (##string? obj2)
                           (##string-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##u8vector? obj1)
                  (profile! 12)
                  (if (and (##u8vector? obj2)
                           (##u8vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##s8vector? obj1)
                  (profile! 13)
                  (if (and (##s8vector? obj2)
                           (##s8vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##u16vector? obj1)
                  (profile! 14)
                  (if (and (##u16vector? obj2)
                           (##u16vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##s16vector? obj1)
                  (profile! 15)
                  (if (and (##s16vector? obj2)
                           (##s16vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##u32vector? obj1)
                  (profile! 16)
                  (if (and (##u32vector? obj2)
                           (##u32vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##s32vector? obj1)
                  (profile! 17)
                  (if (and (##s32vector? obj2)
                           (##s32vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##u64vector? obj1)
                  (profile! 18)
                  (if (and (##u64vector? obj2)
                           (##u64vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##s64vector? obj1)
                  (profile! 19)
                  (if (and (##s64vector? obj2)
                           (##s64vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##f32vector? obj1)
                  (profile! 20)
                  (if (and (##f32vector? obj2)
                           (##f32vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 ((##f64vector? obj1)
                  (profile! 21)
                  (if (and (##f64vector? obj2)
                           (##f64vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 (else
                  (profile! 22)
                  (false)))))))

(macro-case-target

 ((C)

(define ##equal-hint 0)

(##define-macro (macro-equal-hint-get)
  `##equal-hint)

(##define-macro (macro-equal-hint-set! hint)
  `(let ((h ,hint))
     (set! ##equal-hint h)))

(define-prim (##equal? obj1 obj2)

  ;; various parameters to control how much effort is assigned to the fast
  ;; and slow algorithms

  (define fast-bank0    150)
  (define slow-size0    40)
  (define fast-bank1    2000)
  (define limit-growth  4)
  (define hint-bloat    135)
  (define max-used-bank 100000)
  (define max-ht-count  4000)
  (define loads         '#f64(0.0 0.2 0.85))

  (##define-macro (profile! i)
    `#f) ;; disable profiling

  ;; fast equality testing using a time bank to terminate when objects
  ;; have sharing or cycles

  (macro-define-equal-objs?
   fast-equal-objs? (bank)
   #f

   (##define-macro (true) `bank)
   (##define-macro (false) `##min-fixnum)

   (##define-macro (recursion obj1 obj2 tail-expr)
     `(let ((bank (##fx- bank 1)))
        (if (##fx< bank 0)
            bank
            ,tail-expr)))

   (##define-macro (conj equal-obj?-expr tail-expr)
     `(let ((bank ,equal-obj?-expr))
        (if (##fx< bank 0)
            bank
            ,tail-expr))))

  ;; slow equality testing using a hash table to check for sharing and cycles

  (macro-define-equal-objs?
   slow-equal-objs? (ht)
   #f

   (##define-macro (true) `1)
   (##define-macro (false) `0)

   (##define-macro (recursion obj1 obj2 tail-expr)
     `(let ((r (union-find ,obj1 ,obj2 ht)))
        (if (##not (##fx= r (false)))
            r ;; either obj1 & obj2 were in same equiv class or need to abort
            ,tail-expr)))

   (##define-macro (conj equal-obj?-expr tail-expr)
     `(let ((r ,equal-obj?-expr))
        (if (##not (##fx= r (true)))
            r ;; either obj1 & obj2 are not equal or need to abort
            ,tail-expr)))

   ;; union-find algorithm to detect sharing and cycles

   (define (union-find obj1 obj2 ht)
     (let* ((uht (##unbox ht))
            (code (##gc-hash-table-union! uht obj1 obj2)))

       ;; code
       ;; 0    obj1 and obj2 found in ht, and in same equiv class
       ;; 1    obj1 and obj2 found in ht, but not in same equiv class
       ;; 2-3  only one of obj1 and obj2 found in ht (2 = need to grow ht)
       ;; 4-5  neither obj1 or obj2 found in ht (4 = need to grow ht)

       (if (##fx< code 4) ;; code = 0, 1, 2 or 3... keep track of sharing
           (macro-gc-hash-table-min-count-set!
            uht
            (##fx+ 1 (macro-gc-hash-table-min-count uht))))

       (if (##fx= code 0)
           (true)
           (if (##fxodd? code) ;; code = 1, 3 or 5
               (false)
               ;; hash table is full and needs to be grown
               (if (##fx= 0 (macro-gc-hash-table-min-count uht))
                   -1 ;; no sharing found so abort equality testing
                   (let ((new-ht ;; sharing found so keep going
                          (##gc-hash-table-rehash!
                           uht
                           (##gc-hash-table-resize! uht loads))))
                     (##set-box! ht new-ht)
                     (false))))))))

  ;; main equality testing function

  (macro-define-equal-objs?
   main-equal-objs? ()
   recursion-handler

   (##define-macro (true) `#t)
   (##define-macro (false) `#f)

   (define (recursion-handler obj1 obj2)
     (let ((hint (macro-equal-hint-get)))
       (cond ((##fx= hint 0)
              (let* ((bank fast-bank0)
                     (fr (fast-equal-objs? obj1 obj2 bank)))
                (if (##fx>= fr 0) ;; determine if bank was not exhausted
                    (fast-equal-returning-true (##fx- bank fr)) ;; equal
                    (if (##fx= fr ##min-fixnum)
                        (fast-equal-returning-false) ;; not equal
                        (let* ((size slow-size0) ;; exhausted available bank
                               (ht (new-gc-hash-table size))
                               (sr (slow-equal-objs? obj1 obj2 ht)))
                          (if (##fx= sr -1) ;; reached limit, so try fast algo
                              (fast obj1 obj2 fast-bank1)
                              (slow-equal-returning
                               (##not (##fx= sr 0))
                               (macro-gc-hash-table-count (##unbox ht)))))))))
             ((##fx> hint 0)
              (fast obj1 obj2 hint))
             (else
              (slow obj1 obj2 (##fx- hint))))))

   (define (fast obj1 obj2 limit)
     (let* ((bank (##fx* limit-growth limit))
            (fr (fast-equal-objs? obj1 obj2 bank)))
       (if (##fx>= fr 0) ;; determine if bank was not exhausted
           (fast-equal-returning-true (##fx- bank fr)) ;; equal
           (if (##fx= fr ##min-fixnum)
               (fast-equal-returning-false) ;; not equal
               (slow obj1 obj2 limit))))) ;; reached limit, so try slow algo

   (define (fast-equal-returning-false)
     ;; change hint only if currently "slow"
     (if (##fx> (macro-equal-hint-get) 0)
         (macro-equal-hint-set! 0))
     #f)

   (define (fast-equal-returning-true used-bank)
     ;; change hint to "fast" with 135% of used bank
     (let ((new-bank
            (##fxquotient (##fx* (##fxmin used-bank max-used-bank) hint-bloat)
                          100)))
       (macro-equal-hint-set! (##fxmax new-bank fast-bank0))
       #t))

   (define (slow obj1 obj2 limit)
     (let* ((size limit)
            (ht (new-gc-hash-table size))
            (sr (slow-equal-objs? obj1 obj2 ht)))
        (if (##fx= sr -1) ;; reached limit, so try fast algorithm
            (fast obj1 obj2 (##fx* limit-growth limit))
            (slow-equal-returning
             (##not (##fx= sr 0))
             (macro-gc-hash-table-count (##unbox ht))))))

   (define (slow-equal-returning result count)
     ;; change hint to "slow" with 135% of count
     (let ((new-count
            (##fxquotient (##fx* (##fxmin count max-ht-count) hint-bloat)
                          100)))
       (macro-equal-hint-set! (##fx- (##fxmax new-count slow-size0)))
       result))

   (define (new-gc-hash-table size)
     (let ((uht (##gc-hash-table-allocate
                 size
                 (##fxior (macro-gc-hash-table-flag-mem-alloc-keys)
                          (macro-gc-hash-table-flag-union-find))
                 loads)))
       (macro-gc-hash-table-min-count-set! uht 0)
       (##box uht))))

  (main-equal-objs? obj1 obj2))

)

 (else

(define-prim (##equal? obj1 obj2)

  (##define-macro (profile! i)
    `#f) ;; disable profiling

  (macro-define-equal-objs?
   equal-objs? ()
   #f

   (##define-macro (macro-table-hash obj) `#f)
   (##define-macro (macro-table-gcht obj) `#f)

   (##define-macro (true) `#t)
   (##define-macro (false) `#f)

   (##define-macro (recursion obj1 obj2 tail-expr)
     tail-expr)

   (##define-macro (conj equal-obj?-expr tail-expr)
     `(and ,equal-obj?-expr ,tail-expr)))

  (equal-objs? obj1 obj2))

))

(define-prim (equal? obj1 obj2)
  (##equal? obj1 obj2))
