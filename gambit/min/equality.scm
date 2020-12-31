;;;----------------------------------------------------------------------------

;;; Object equality.

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
           `(#;(define (table-equal obj1 obj2 ,@params)
               (conj (gc-hash-table-equal (macro-table-gcht obj1)
                                          obj2
                                          ,@params)
                     (if (macro-table-test obj1)
                         (gc-hash-table-equal (macro-table-hash obj1)
                                              obj2
                                              ,@params)
                         (true))))

             #;(define (gc-hash-table-equal ht1 table2 ,@params)
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

             #;(define (structure-equal obj1 obj2 type len ,@params)
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
                 ((##char? obj1)
                  (and (##char? obj2)
                       (##inline-host-expression "(@1@).code === (@2@).code" obj1 obj2)))
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
              #;   ((macro-table? obj1)
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
                #; ((##structure? obj1)
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
#;                 ((##box? obj1)
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
              #;   ((##s8vector? obj1)
                  (profile! 13)
                  (if (and (##s8vector? obj2)
                           (##s8vector-equal? obj1 obj2))
                      (true)
                     (false)))
              #;   ((##u16vector? obj1)
                  (profile! 14)
                  (if (and (##u16vector? obj2)
                           (##u16vector-equal? obj1 obj2))
                      (true)
                      (false)))
            #;     ((##s16vector? obj1)
                  (profile! 15)
                  (if (and (##s16vector? obj2)
                           (##s16vector-equal? obj1 obj2))
                      (true)
                      (false)))
             #;    ((##u32vector? obj1)
                 ; (profile! 16)
                  (if (and (##u32vector? obj2)
                           (##u32vector-equal? obj1 obj2))
                      (true)
                      (false)))
              #;   ((##s32vector? obj1)
                  ;(profile! 17)
                  (if (and (##s32vector? obj2)
                           (##s32vector-equal? obj1 obj2))
                      (true)
                      (false)))
            #;     ((##u64vector? obj1)
                  (profile! 18)
                  (if (and (##u64vector? obj2)
                           (##u64vector-equal? obj1 obj2))
                      (true)
                      (false)))
          #;       ((##s64vector? obj1)
                  (profile! 19)
                  (if (and (##s64vector? obj2)
                           (##s64vector-equal? obj1 obj2))
                      (true)
                      (false)))
           #;      ((##f32vector? obj1)
                  (profile! 20)
                  (if (and (##f32vector? obj2)
                           (##f32vector-equal? obj1 obj2))
                      (true)
                      (false)))
            #;     ((##f64vector? obj1)
                  (profile! 21)
                  (if (and (##f64vector? obj2)
                           (##f64vector-equal? obj1 obj2))
                      (true)
                      (false)))
                 (else
                  (profile! 22)
                  (false)))))))

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

(define-prim (equal? obj1 obj2)
  (##equal? obj1 obj2))
