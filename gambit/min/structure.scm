;;;----------------------------------------------------------------------------

;;; Structure support.

;; For bootstraping purposes the type of type objects must be
;; explicitly constructed.  It is as though the following form had
;; been used:
;;
;;   (define-type type
;;     id: ...special-type...
;;     (id      unprintable: equality-test:)
;;     (name    unprintable: equality-skip:)
;;     (flags   unprintable: equality-skip:)
;;     (super   unprintable: equality-skip:)
;;     (fields  unprintable: equality-skip:)
;;   )

(##define-macro (macro-type-type-constant)
  (let ((type-type
         (##structure
          #f ;; this structure's type descriptor is itself! (set later)
          '##type-5
          'type
          '8
          '#f
          '#(id 1 #f name 5 #f flags 5 #f super 5 #f fields 5 #f))))
    (##structure-type-set! type-type type-type) ;; self reference
    `',type-type))

(define ##type-type (macro-type-type-constant))

(define-prim (##type-id type)
  (##unchecked-structure-ref type 1 ##type-type ##type-id))

(define-prim (##type-name type)
  (##unchecked-structure-ref type 2 ##type-type ##type-name))

(define-prim (##type-flags type)
  (##unchecked-structure-ref type 3 ##type-type ##type-flags))

(define-prim (##type-super type)
  (##unchecked-structure-ref type 4 ##type-type ##type-super))

(define-prim (##type-fields type)
  (##unchecked-structure-ref type 5 ##type-type ##type-fields))

(define-prim (##structure-direct-instance-of? obj type-id)
  (and (##structure? obj)
       (##eq? (##type-id (##structure-type obj))
              type-id)))

(define-prim (##structure-instance-of? obj type-id)
  (and (##structure? obj)
       (let loop ((c (##structure-type obj)))
         (if (##eq? (##type-id c) type-id)
           #t
           (let ((super (##type-super c)))
             (and super
                  (loop super)))))))

(define-prim (##type? obj)
  (##structure-direct-instance-of? obj (##type-id ##type-type)))

(define-prim (##structure-type obj)
  (##vector-ref obj 0))

(define-prim (##structure-type-set! obj type)
  (##vector-set! obj 0 type))

(define-prim (##make-structure type len)
  (let ((s (##make-vector len)))
    (##subtype-set! s (macro-subtype-structure))
    (##vector-set! s 0 type)
    s))

(define-prim (##structure-length obj)
  (##vector-length obj))

(define-prim (##structure type . fields)

  (define (make-struct fields i)
    (if (##pair? fields)
        (let ((s (make-struct (##cdr fields) (##fx+ i 1))))
          (##unchecked-structure-set! s (##car fields) i type #f)
          s)
        (##make-structure type i)))

  (make-struct fields 1))

(define-prim (##structure-ref obj i type proc)
  (if (##structure-instance-of? obj (##type-id type))
    (##unchecked-structure-ref obj i type proc)
    (##raise-type-exception
     1
     type
     (if proc proc ##structure-ref)
     (if proc (##list obj) (##list obj i type proc)))))

(define-prim (##structure-set! obj val i type proc)
  (if (##structure-instance-of? obj (##type-id type))
    (begin
      (##unchecked-structure-set! obj val i type proc)
      (##void))
    (##raise-type-exception
     1
     type
     (if proc proc ##structure-set!)
     (if proc (##list obj val) (##list obj val i type proc)))))

(define-prim (##structure-set obj val i type proc)
  (if (##structure-instance-of? obj (##type-id type))
    (let ((result (##structure-copy obj)))
      (##unchecked-structure-set! result val i type proc)
      result)
    (##raise-type-exception
     1
     type
     (if proc proc ##structure-set)
     (if proc (##list obj val) (##list obj val i type proc)))))

(define-prim (##structure-cas! obj val oldval i type proc)
  (if (##structure-instance-of? obj (##type-id type))
    (begin
      (##unchecked-structure-cas! obj val oldval i type proc)
      (##void))
    (##raise-type-exception
     1
     type
     (if proc proc ##structure-cas!)
     (if proc (##list obj val oldval) (##list obj val oldval i type proc)))))

(define-prim (##direct-structure-ref obj i type proc)
  (if (##structure-direct-instance-of? obj (##type-id type))
    (##unchecked-structure-ref obj i type proc)
    (##raise-type-exception
     1
     type
     (if proc proc ##direct-structure-ref)
     (if proc (##list obj) (##list obj i type proc)))))

(define-prim (##direct-structure-set! obj val i type proc)
  (if (##structure-direct-instance-of? obj (##type-id type))
    (begin
      (##unchecked-structure-set! obj val i type proc)
      (##void))
    (##raise-type-exception
     1
     type
     (if proc proc ##direct-structure-set!)
     (if proc (##list obj val) (##list obj val i type proc)))))

(define-prim (##direct-structure-set obj val i type proc)
  (if (##structure-direct-instance-of? obj (##type-id type))
    (let ((result (##structure-copy obj)))
      (##unchecked-structure-set! result val i type proc)
      result)
    (##raise-type-exception
     1
     type
     (if proc proc ##direct-structure-set)
     (if proc (##list obj val) (##list obj val i type proc)))))

(define-prim (##direct-structure-cas! obj val oldval i type proc)
  (if (##structure-direct-instance-of? obj (##type-id type))
    (begin
      (##unchecked-structure-cas! obj val oldval i type proc)
      (##void))
    (##raise-type-exception
     1
     type
     (if proc proc ##direct-structure-cas!)
     (if proc (##list obj val oldval) (##list obj val oldval i type proc)))))

(define-prim (##unchecked-structure-ref obj i type proc))

(define-prim (##unchecked-structure-set! obj val i type proc))

(define-prim (##unchecked-structure-cas! obj val oldval i type proc)
  ;; TODO: remove after bootstrap
  (##vector-cas! obj i val oldval))

(define-prim (##structure-copy obj)
  (let* ((len (##structure-length obj))
         (type (##structure-type obj))
         (result (##make-structure type len)))
    (let loop ((i (##fx- len 1)))
      (if (##fx> i 0)
          (begin
            (##unchecked-structure-set!
             result
             (##unchecked-structure-ref obj i type ##structure-copy)
             i
             type
             ##structure-copy)
            (loop (##fx- i 1)))
          result))))

;;;----------------------------------------------------------------------------
