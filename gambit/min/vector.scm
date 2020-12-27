(define-prim-vector-procedures
  vector
  0
  macro-no-force
  macro-no-check
  macro-no-check
  #f
  #f
  define-map-and-for-each
  ##equal?)
(define-prim-vector-procedures
  u8vector
  0
  macro-force-vars
  macro-check-exact-unsigned-int8
  macro-check-exact-unsigned-int8-list
  macro-test-exact-unsigned-int8
  ##fail-check-exact-unsigned-int8
  #f
  ##fx=)

(define bytevector?        u8vector?)
(define make-bytevector    make-u8vector)
(define bytevector         u8vector)
(define bytevector-length  u8vector-length)
(define bytevector-u8-ref  u8vector-ref)
(define bytevector-u8-set! u8vector-set!)
(define bytevector-copy    u8vector-copy)
(define bytevector-copy!   u8vector-copy!)
(define bytevector-append  u8vector-append)

;; (##include "~~/lib/gambit/vector/vector.scm")
