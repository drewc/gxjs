(declare (extended-bindings))
#;(declare (extended-bindings standard-bindings
                            ))

;; (##inline-host-statement "console.error('make-vector', @1@)" make-vector)

(define (test> name i pred j)
  (let ((res (pred i j)))
    (##inline-host-statement "
     (() => {
      function hst (thing) {
         try { return RTS.scm2host(thing) } catch { return thing } ;
     };
     const name = hst(@1@);
     const i = hst(@2@);
     const j = hst(@3@);
     const res = (@4@);
     const msg = name + ' ' + JSON.stringify(i) + ' => ' + JSON.stringify(j);
     if (res) {
       console.log('Success:', msg)
     } else {
       console.error('Failure', msg)
     }
   })()

" name i j res)))

(define (test-prim-vector)
  (let ((v (vector 1 2 3)))
    (test> "vector? #t" (vector? v) eq? #t)
    (test> "vector" v eq? v)
    (test> "make-vector" (make-vector 5) (lambda (a b) (vector? a)) #t)
    (test> "vector-length" (vector-length (make-vector 42)) ##fx= 42)
    (test> "vector-set!" (let ((v (vector 0 1 2)))
                           (vector-set! v 2 42)
                           (vector-ref #(0 1 42) 2))
           ##fx= 42)
    (test> "vector-shrink!" (let ((v (vector 0 1 2 3 4)))
                              (vector-shrink! v 1)
                              (vector-length v))
           ##fx= 1)))

(define (test-prim-list)
 (let* ((l1 (list #t #f 3 4))
        (l2 (list))
        (c1 (cons 4 2))
        (c2 (cons (cons 4 2) (cons 8 6)))
        (c3 (cons 16 (cons 32 c2))))

   (test> "not null?" (null? l1) eq? #f)
   (test> "null?" (null? l2) eq? #t)
   (test> "not pair?" (pair? l2) eq? #f)
   (test> "pair?" (pair? l1) eq? #t)

   (test> "car" (car c1) ##fx= 4)
   (test> "cdr" (cdr c1) ##fx= 2)
   (test> "cadr" (cadr l1) eq? #f)
   (test> "cdar" (cdar c2) ##fx= 2)
   (test> "cddr" (cddr c2) ##fx= 6)
   (test> "cdddr" (car (cdddr c3)) ##fx= 8)
   (test> "caddr" (pair? (caddr c3)) eq? #t)
   (test> "set-car!" (let ((_ (set-car! c1 7))) (car c1))
          ##fx= 7)
   (test> "set-cdr!" (let ((_ (set-cdr! c1 42))) (cdr c1))
          ##fx= 42)
   (test> "void" (list (##void) (void) #!void) eq? #t)

   1234

   ))

(define (test-tables)
  (let ((tbl (make-table)))
    (test> "table?" (table? tbl) eq? #t)))

(define (test-univ)
 (test> "eq?" 'foobar eq? 'foobar)
 (test> "eq? #t #t" #t eq? #t)

 (test-prim-vector)
 (test-prim-list)

 (test> "string-append" (string-append "asd" "qwe") string=? "asdqwe")

 (test-tables)

 (test> "fixnum?" (fixnum? 1) eq? #t)
 )

(##inline-host-statement "module.exports = RTS.scm2host(@1@);" (lambda () (test-univ)))
