;;; Copyright (c) 2021 by Drew Crampsie, All Rights Reserved.
;;; Copyright (c) 1994-2020 by Marc Feeley, All Rights Reserved.
(declare (extended-bindings))
(namespace (""))

(define-prim (##eq? obj1 obj2))
(define-prim (eq? obj1 obj2) (macro-force-vars (obj1 obj2) (##eq? obj1 obj2)))
(define-prim (##fx= o1 o2))
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-prim (##identity x)
  x)

(define-prim (identity x)
  x)

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-prim (##void))

(define-prim (void)
  (##void))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-prim (##absent-object)
  (macro-absent-obj))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;;;----------------------------------------------------------------------------

;;; Jobs.

(define-prim (##make-jobs)
  (macro-make-fifo))

(define-prim (##add-job-at-tail! jobs job)
  (macro-fifo-insert-at-tail! jobs job))

(define-prim (##add-job! jobs job)
  (macro-fifo-insert-at-head! jobs job))

(define-prim (##execute-jobs! jobs)
  (let loop ((lst (macro-fifo->list jobs)))
    (if (##pair? lst)
      (begin
        ((##car lst))
        (loop (##cdr lst))))))

(define-prim (##execute-and-clear-jobs! jobs)
  (let loop ((lst (macro-fifo-remove-all! jobs)))
    (if (##pair? lst)
      (begin
        ((##car lst))
        (loop (##cdr lst))))))

(define-prim (##clear-jobs! jobs)
  (macro-fifo-remove-all! jobs)
  (##void))

;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------

;;; Process exit.

(define-prim (##exit-with-err-code-no-cleanup err-code)
  (##declare (not interrupts-enabled))
  (macro-case-target

   ((js)
    (##inline-host-statement
     "
      var code = RTS.scm2host(@1@);
      if ((function () { return this !== this.window; })()) { // nodejs?
        process.exit(code);
      } else {
        throw Error('process exiting with code=' + code);
      }
     "
     (##fx- err-code 1)))

   ((python)
    (##inline-host-statement "exit(@1@)" (##fx- err-code 1)))

   (else
    (println "unimplemented ##exit-with-err-code-no-cleanup called with err-code=")
    (println err-code))))

(define (##execute-final-wills!)
  ;; do nothing because wills are only implemented in C backend
  #f)

(define (##exit-trampoline)
  (##declare (not interrupts-enabled))
  (macro-case-target

   ((js)
    (##inline-host-statement "g_r0 = null;"))

   ((python)
    (##inline-host-statement "g_r0 = None"))

   (else
    (println "unimplemented ##exit-trampoline called"))))

;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------

;;; Program startup and exit. from lib/_kernel.scm

(define ##exit-jobs (##make-jobs))

;;; (##add-exit-job! thunk) can be called to add a job to
;;; do when the program exits.  (##clear-exit-jobs!) clears the jobs.

(define-prim (##add-exit-job! thunk)
  (##add-job! ##exit-jobs thunk))

(define-prim (##clear-exit-jobs!)
  (##clear-jobs! ##exit-jobs))

(define ##cleaning-up? #f)

(define-prim (##exit-cleanup)
  (let ((is-in-cleanup? ##cleaning-up?))
    (set! ##cleaning-up? #t) ;; only do cleanup once
    (if (##not is-in-cleanup?)
        (begin
          (##execute-and-clear-jobs! ##exit-jobs)
          (##execute-final-wills!)))))

(define-prim (##exit-with-err-code err-code)
  (##exit-cleanup)
  (##exit-with-err-code-no-cleanup err-code))

(define-prim (##exit #!optional (status (macro-EXIT-CODE-OK)))
  (##exit-with-err-code (##fx+ status 1)))

(define-prim (##exit-abruptly #!optional (status (macro-EXIT-CODE-SOFTWARE)))
  (##exit-with-err-code-no-cleanup (##fx+ status 1)))

(define-prim (##exit-with-exception exc)
  (##exit (macro-EXIT-CODE-SOFTWARE)))

(define (##interrupt-vector-set! code handler) #f)

(define-prim (##intr-terminate-handler-set! handler)
  (##interrupt-vector-set! 1 handler)) ;; ___INTR_TERMINATE

(define ##feature-intr-terminate
  (##intr-terminate-handler-set! ##exit-abruptly))



;;----------------------------------------------------------------------------

;; Argument list transformation used when some exceptions are raised.

(define-prim (##argument-list-remove-absent! lst tail)
  (let loop ((lst1 tail)
             (lst2 #f)
             (lst3 lst))
    (if (##pair? lst3)
      (let ((val (##car lst3)))
        (if (##eq? val (macro-absent-obj))
          (loop lst1
                lst2
                (##cdr lst3))
          (loop (if lst2
                  (begin
                    (##set-cdr! lst2 lst3)
                    lst1)
                  lst3)
                lst3
                (##cdr lst3))))
      (begin
        (if lst2
          (##set-cdr! lst2 tail))
        lst1))))

(define-prim (##argument-list-remove-absent-keys! lst)
  (let loop ((lst1 #f)
             (lst2 #f)
             (lst3 lst))
    (if (and (##pair? lst3) (##keyword? (##car lst3)))
      (let ((val (##cadr lst3)))
        (if (##eq? val (macro-absent-obj))
          (loop lst1
                lst2
                (##cddr lst3))
          (loop (if lst2
                  (begin
                    (##set-cdr! lst2 lst3)
                    lst1)
                  lst3)
                (##cdr lst3)
                (##cddr lst3))))
      (let ((tail (if (##pair? lst3) (##car lst3) '())))
        (if lst2
          (begin
            (##set-cdr! lst2 tail)
            lst1)
          tail)))))

(define-prim (##argument-list-fix-rest-param! lst)
  (let loop ((curr #f) (next lst))
    (let ((tail (##cdr next)))
      (if (##pair? tail)
        (loop next tail)
        (if curr
          (begin
            (##set-cdr! curr (##car next))
            lst)
          (##car next))))))

(define-prim (##extract-procedure-and-arguments proc args val1 val2 val3 cont)
  (cond ((##null? proc)
         (cont (##car args)
               (##argument-list-remove-absent!
                (##argument-list-fix-rest-param! (##cdr args))
                '())
               val1
               val2
               val3))
        ((##pair? proc)
         (cont (##car proc)
               (##argument-list-remove-absent!
                args
                (##argument-list-remove-absent-keys! (##cdr proc)))
               val1
               val2
               val3))
        (else
         (cont proc
               (##argument-list-remove-absent! args '())
               val1
               val2
               val3))))

;;;----------------------------------------------------------------------------

(implement-library-type-type-exception)

(define-prim (##raise-type-exception arg-num type-id proc args)
  (##extract-procedure-and-arguments
   proc
   args
   arg-num
   type-id
   #f
   (lambda (procedure arguments arg-num type-id dummy)
     (##inline-host-statement
      "console.error('ERROR: Gambit Type Exception:', (@1@).name,
       'argument', RTS.list2vector(@2@)[@3@ - 1],
         'is not of type', (@4@).name);
"
      proc args arg-num type-id)
     (macro-raise
      (macro-make-type-exception procedure arguments arg-num type-id)))))

(define (##apply proc arg1 . rest)
  (declare (not inline))
  (if (##pair? rest)

    (let loop ((prev arg1) (lst rest))
      (let ((temp (##car lst)))
        (##set-car! lst prev)
        (let ((tail (##cdr lst)))
          (if (##pair? tail)
            (loop temp tail)
            (begin
              (##set-cdr! lst temp)
              (##apply proc rest))))))

    (##apply proc arg1)))

(define-prim (apply proc arg1 . rest)
  (if (##pair? rest)
    (##apply ##apply proc (cons arg1 rest))
    (##apply proc arg1)))

(##inline-host-declaration  "
RTS.list2vector = function (list) {
   const vec = [];
   function l2v (cons) {
      if (cons === null) {
         return vec;
      } else {
        vec.push(cons.car);
        return l2v(cons.cdr)
      }
   }
   return l2v(list);
};

")
(define-prim (##list->vector lst)
  (##inline-host-expression " RTS.list2vector(@1@); " lst))

(define-prim (list-vector lst) (##list->vector lst))

;; AUTOMAGIC:  vector?, vector-length, vector-ref, vector-set!

(define-prim (##vector . lst) (##list->vector lst))
(define-prim (vector . lst) (##list->vector lst))

(define-prim (##make-vector k #!optional (fill 0))
  (##make-vector k fill))
(define-prim (make-vector arg1 #!optional (arg2 #f))
  (macro-force-vars (arg1 arg2) (##make-vector arg1 arg2)))

(define-prim (##vector-shrink! arg1 arg2))
(define-prim (vector-shrink! arg1 arg2) (##vector-shrink! arg1 arg2))

(define-prim (##make-string k #!optional (fill #\null))
  (##make-string k fill))
(define-prim make-string ##make-string)
(define-prim (##string-append . strs)
  (let ((s (##make-string 0)))
    (let app ((ss strs))
      (if (##null? ss) s
          (begin
            (##inline-host-statement #<<EOF
 (() => {
   const s = (@1@), t = (@2@);
   t.codes.map(c => s.codes.push(c))
 })();
EOF
s (car ss))
          (app (cdr ss)))))))

(define-prim (string-append . strs) (apply ##string-append strs))

(define-prim (##string=? s . strs)
  (let lp ((ss strs))
    (if (##null? ss) #t
        (begin
          (and (##inline-host-expression "((a, b) => {
return Array.isArray(a) && Array.isArray(b) && a.length === b.length &&
    a.every((val, index) => val === b[index]);
})((@1@).codes, (@2@).codes);" s (car ss))
               (lp (cdr ss)))))))
(define-prim (string=? . ss) (##apply ##string=? ss))

(##include "~~lib/_system#.scm")
;;;----------------------------------------------------------------------------

;;; Tables.

;;; imports:
;;; from _kernel.scm
;;;    (##extract-procedure-and-arguments ...)
;;;    (##raise-type-exception ...)
;;; from _equal.scm
;;;    (##equal? ...)
;;; from _std.scm
;;;    (##length ...)
;;;    (##map ...)
;;;    (##fail-check-procedure ...)

;;; exports:
;;;    (##fail-check-table ...)
;;;    (##fail-check-unbound-key-exception ...)
;;;    (##list->table-aux ...)
;;;    (##make-table-aux ...)
;;;    (##raise-unbound-key-exception ...)
;;;    (##table->list ...)
;;;    (##table-copy ...)
;;;    (##table-length ...)
;;;    (##table-ref ...)
;;;    (##table-search ...)
;;;    (##table-set! ...)
;;;    (list->table ...)
;;;    (make-table ...)
;;;    (table->list ...)
;;;    (table-copy ...)
;;;    (table-length ...)
;;;    (table-ref ...)
;;;    (table-set! ...)
;;;    (table? ...)
;;;    (unbound-key-exception-arguments ...)
;;;    (unbound-key-exception-procedure ...)
;;;    (unbound-key-exception? ...)

;;;----------------------------------------------------------------------------

(implement-type-table)

(define-fail-check-type table (macro-type-table))

(define-check-type table (macro-type-table)
  macro-table?)

(implement-library-type-unbound-key-exception)

(define-prim (##raise-unbound-key-exception proc . args)
  (##extract-procedure-and-arguments
   proc
   args
   #f
   #f
   #f
   (lambda (procedure arguments dummy1 dummy2 dummy3)
     (macro-raise
      (macro-make-unbound-key-exception
       procedure
       arguments)))))

(define-prim (##table? obj)
  (macro-table? obj))

(define-prim (table? obj)
  (macro-table? obj))

;;;----------------------------------------------------------------------------

(define-prim (##make-table-aux
              #!optional
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys #f)
              (weak-values #f)
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))

  (define (check-test arg-num)
    (if (##eq? test (macro-absent-obj))
      (checks-done ##equal?
                   arg-num)
      (let ((arg-num (##fx+ arg-num 2)))
        (macro-check-procedure
         test
         arg-num
         (make-table size: size
                     init: init
                     weak-keys: weak-keys
                     weak-values: weak-values
                     test: test
                     hash: hash
                     min-load: min-load
                     max-load: max-load)
         (checks-done test
                      arg-num)))))

  (define (checks-done test-fn arg-num)
    (macro-make-table (if (or (##eq? test-fn eq?)
                              (##eq? test-fn ##eq?))
                          #f
                          test-fn)
                      init
                      ;; weak-keys/values are extended booleans
                      (##univ-table-make-hashtable (##not (##not weak-keys))
                                                   (##not (##not weak-values)))
                      (##fx+ (if weak-keys 1 0)
                             (if weak-values 2 0))
))

  (check-test 0))

(define-prim (##make-table
              #!key
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys (macro-absent-obj))
              (weak-values (macro-absent-obj))
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))
  (##make-table-aux
   size
   init
   weak-keys
   weak-values
   test
   hash
   min-load
   max-load))

(define-prim (make-table
              #!key
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys (macro-absent-obj))
              (weak-values (macro-absent-obj))
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))
  (##make-table-aux
   size
   init
   weak-keys
   weak-values
   test
   hash
   min-load
   max-load))

(define-prim (##table-find-key
              table
              key
              #!optional
              (found (lambda (key) key))
              (not-found (lambda () #!void)))
  (let ((test (macro-table-test table)))
    (let loop ((keys (##univ-table-keys (macro-table-hashtable table))))
      (cond
       ((##null? keys)
        (not-found))
       ((test (##car keys) key)
        (found (##car keys)))
       (else
        (loop (##cdr keys)))))))

(define-prim (##table-ref
              table
              key
              #!optional
              (default-value (macro-absent-obj)))

  (let ((test (macro-table-test table)))
    (define (found key)
      (##univ-table-ref (macro-table-hashtable table) key))
    (define (not-found)
      (cond
       ((##not (##eq? default-value (macro-absent-obj)))
        default-value)
       ((##not (##eq? (macro-table-init table) (macro-absent-obj)))
        (macro-table-init table))
       (else
        (##raise-unbound-key-exception
         table-ref
         table
         key))))
    (cond
     (test ;; not and eq?-table
      (##table-find-key table key found not-found))
     ((##univ-table-key-exists? (macro-table-hashtable table) key)
      (found key))
     (else
      (not-found)))))

#;(define-prim (table-ref
              table
              key
              #!optional
              (default-value (macro-absent-obj)))
  (macro-force-vars (table key default-value)
    (macro-check-table table 1 (table-ref table key default-value)
      (##table-ref table key default-value))))

(define-prim (table-ref
              table
              key
              #!optional
              (default-value (macro-absent-obj)))
  (##table-ref table key default-value))
(define-prim (##table-set!
              table
              key
              #!optional
              (val (macro-absent-obj)))

  (let ((test (macro-table-test table)))
    (if (macro-table-test table) ;; if it's not an eq?-table
        (##table-find-key table key
                          (lambda (k)
                            (##univ-table-delete (macro-table-hashtable table) k))))

    (if (##eq? val (macro-absent-obj))
        (##univ-table-delete (macro-table-hashtable table) key)
        (##univ-table-set! (macro-table-hashtable table)
                           key
                           val))))
(define-prim (##table-set!
              table
              key
              #!optional
              (val (macro-absent-obj)))

  (let ((test (macro-table-test table)))
    (if (macro-table-test table) ;; if it's not an eq?-table
        (##table-find-key table key
                          (lambda (k)
                            (##univ-table-delete (macro-table-hashtable table) k))))

    (if (##eq? val (macro-absent-obj))
        (##univ-table-delete (macro-table-hashtable table) key)
        (##univ-table-set! (macro-table-hashtable table)
                           key
                           val))))

(define-prim (table-set!
              table
              key
              #!optional
              (val (macro-absent-obj)))
  (macro-force-vars (table key val)
    (macro-check-table table 1 (table-set! table key val)
      (##table-set! table key val))))


(define-prim (##table-length table)
  (##univ-table-length (macro-table-hashtable table)))

(define-prim (table-length table)
  (macro-force-vars (table)
    (macro-check-table table 1 (table-length table)
      (##table-length table))))

(define-prim (##table->list table)
  (let ((hashtable (macro-table-hashtable table)))
    (map (lambda (key)
           (cons key (##univ-table-ref hashtable key)))
         (##univ-table-keys (macro-table-hashtable table)))))

(define-prim (table->list table)
  (macro-force-vars (table)
    (macro-check-table table 1 (table->list table)
      (##table->list table))))

(define-prim (##list->table-aux
              lst
              #!optional
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys (macro-absent-obj))
              (weak-values (macro-absent-obj))
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))
  (let ((table
         (##make-table-aux
          size
          init
          weak-keys
          weak-values
          test
          hash
          min-load
          max-load)))
    (let loop ((x lst))
      (if (##pair? x)
          (let ((couple (##car x)))
            (macro-check-pair-list
             couple
             1
             (list->table lst
                          size: size
                          init: init
                          weak-keys: weak-keys
                          weak-values: weak-values
                          test: test
                          hash: hash
                          min-load: min-load
                          max-load: max-load)
             (##univ-table-set! (macro-table-hashtable table)
                                (##car couple)
                                (##cdr couple)))
            (loop (##cdr x)))
          (macro-check-list
           x
           1
           (list->table lst
                        size: size
                        init: init
                        weak-keys: weak-keys
                        weak-values: weak-values
                        test: test
                        hash: hash
                        min-load: min-load
                        max-load: max-load)
           table)))))

(define-prim (##list->table
              lst
              #!key
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys (macro-absent-obj))
              (weak-values (macro-absent-obj))
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))
  (##list->table-aux lst))

(define-prim (list->table
              lst
              #!key
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys (macro-absent-obj))
              (weak-values (macro-absent-obj))
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))
  (##list->table-aux lst))

(define-prim (##table-copy table)
  (let ((copy (##make-table-aux
               (macro-absent-obj) ;; size
               (macro-table-init table) ;; init
               (##fxand 1 (macro-table-flags table)) ;; weak-keys
               (##fxand 2 (macro-table-flags table)) ;; weak-values
               (or (macro-table-test table) ##eq?) ;; test
               (macro-absent-obj) ;; hash
               (macro-absent-obj) ;; min-load
               (macro-absent-obj)))) ;; max-load
    (for-each
     (lambda (pair)
       (##table-set! copy (##car pair) (##cdr pair)))
     (##table->list table))
    copy))

(define-prim (table-copy table)
  (macro-force-vars (table)
    (macro-check-table table 1 (table-copy table)
      (##table-copy table))))

(define-prim (##table-search proc table)
  (let loop ((lst (##table->list table)))
    (if (##pair? lst)
        (let ((pair (##car lst)))
          (or (proc (##car pair) (##cdr pair))
              (loop (##cdr lst))))
        #f)))

(define-prim (table-search proc table)
  (##table-search proc table))

(define-prim (##table-for-each proc table)
  (let loop ((lst (##table->list table)))
    (if (##pair? lst)
        (let ((pair (##car lst)))
          (proc (##car pair) (##cdr pair))
          (loop (##cdr lst)))
        #!void)))

(define-prim (table-for-each proc table)
  (##table-for-each proc table))


(##include "~~lib/gambit/list/list#.scm")

(define-prim (##null? obj)
  (##eq? obj '()))

(define-prim (null? obj)
  (macro-force-vars (obj)
    (##null? obj)))

(define-prim (##pair? obj))

(define-prim (pair? obj)
  (macro-force-vars (obj)
    (##pair? obj)))

(define-prim (##cons obj1 obj2))

(define-prim (cons obj1 obj2)
  (##cons obj1 obj2))

(define-prim (##list . lst)
  lst)

(define-prim (list . lst)
  lst)

(define-prim (##set-car! pair val))

(define-prim (set-car! pair val)
  (macro-force-vars (pair)
    (macro-check-pair pair 1 (set-car! pair val)
      (macro-check-mutable pair 1 (set-car! pair val)
        (begin
          (##set-car! pair val)
          (##void))))))

(define-prim (##set-cdr! pair val))

(define-prim (set-cdr! pair val)
  (macro-force-vars (pair)
    (macro-check-pair pair 1 (set-cdr! pair val)
      (macro-check-mutable pair 1 (set-cdr! pair val)
        (begin
          (##set-cdr! pair val)
          (##void))))))

(define-prim (##list? lst)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  ;; This procedure may get into an infinite loop if another thread
  ;; mutates "lst" (if lst1 and lst2 each point to disconnected cycles).

  (let loop ((lst1 lst) (lst2 lst))
    (macro-force-vars (lst1)
      (if (not (pair? lst1))
          (null? lst1)
          (let ((lst1 (cdr lst1)))
            (macro-force-vars (lst1 lst2)
              (cond ((eq? lst1 lst2)
                     #f)
                    ((not (pair? lst2))
                     ;; this case is possible if other threads mutate the list
                     (null? lst2))
                    ((pair? lst1)
                     (loop (cdr lst1) (cdr lst2)))
                    (else
                     (null? lst1)))))))))

(define-prim (list? lst)
  (##list? lst))

(define-prim (##length lst)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (let loop ((x lst) (n 0))
    (if (pair? x)
        (loop (cdr x) (fx+ n 1))
        n)))

(define-prim (length lst)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
  (namespace ("" length)) ;; but not length to ##length

  (let loop ((x lst) (n 0))
    (macro-force-vars (x)
      (if (pair? x)
          (loop (cdr x) (fx+ n 1))
          (macro-check-list x 1 (length lst)
            n)))))
;;;----------------------------------------------------------------------------

(define ##allow-length-mismatch? #t)

(define-prim (##allow-length-mismatch?-set! x)
  (set! ##allow-length-mismatch? x))

(define (##proper-list-length lst)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (let loop ((lst lst) (n 0))
    (macro-force-vars (lst)
      (cond ((pair? lst)
             (loop (cdr lst) (fx+ n 1)))
            ((null? lst)
             n)
            (else
             #f)))))

(define (##cars lsts end)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (define (cars lsts end) ;; assumes lsts is a list of pairs
    (if (pair? lsts)
        (let ((lst1 (car lsts)))
          (macro-force-vars (lst1)
            (cons (car lst1)
                  (cars (cdr lsts) end))))
        end))

  (cars lsts end))

(define (##cdrs lsts)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (define (cdrs lsts)
    (if (pair? lsts)
        (let ((tail (cdrs (cdr lsts))))

          ;; tail is either
          ;; 1) () : (cdr lsts) is ()
          ;; 2) #f : all the elements of (cdr lsts) are not pairs
          ;; 3) a pair : all the elements of (cdr lsts) are pairs
          ;; 4) a fixnum >= 0 : at least one of (cdr lsts) is ()
          ;;                    and at index tail of (cdr lsts) is a pair
          ;; 5) a fixnum < 0 : at least one of (cdr lsts) is not a pair and
          ;;                   at index tail - ##min-fixnum of (cdr lsts) is
          ;;                   the first element that is neither a pair or ()

          (let ((lst1 (car lsts)))
            (macro-force-vars (lst1)
              (cond ((pair? lst1)
                     (cond ((fixnum? tail)
                            (if (fx< tail 0)
                                (fx+ tail 1)
                                0))
                           ((not tail)
                            (if ##allow-length-mismatch?
                                #f
                                0))
                           (else
                            (cons (cdr lst1) tail))))
                    ((null? lst1)
                     (cond ((fixnum? tail)
                            (fx+ tail 1))
                           ((pair? tail)
                            (if ##allow-length-mismatch?
                                #f
                                1))
                           (else
                            #f)))
                    (else
                     ##min-fixnum)))))
        '()))

  (cdrs lsts))

(define-prim (##map proc x . y)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (define (map-1 x)

    (define (map-1 lst1)
      (if (pair? lst1)
          (let* ((result (proc (car lst1)))
                 (tail (map-1 (cdr lst1))))
            (cons result tail))
          '()))

    (map-1 x))

  (define (map-n x-y)

    (define (map-n lsts)
      (let ((rests (##cdrs lsts)))
        (if (not rests)
            '()
            (if (pair? rests)
                (let* ((args (##cars lsts '()))
                       (result (apply proc args))
                       (tail (map-n rests)))
                  (cons result tail))
                '()))))

    (map-n x-y))

  (if (null? y)
      (map-1 x)
      (map-n (cons x y))))

(define-prim (map proc x . y)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
  (namespace ("" map)) ;; but not map to ##map

  (macro-force-vars (proc)
    (macro-check-procedure proc 1 (map proc x . y)
      (let ()

        (define (map-1 x)

          (define (map-1 lst1)
            (macro-force-vars (lst1)
              (if (pair? lst1)
                  (let* ((result (proc (car lst1)))
                         (tail (map-1 (cdr lst1))))
                    (macro-if-checks
                     (and tail
                          (cons result tail))
                     (cons result tail)))
                  (macro-if-checks
                   (if (null? lst1)
                       '()
                       #f)
                   '()))))

          (macro-if-checks
           (let ((result (map-1 x)))
             (or result
                 (macro-fail-check-list
                  2
                  (map proc x))))
           (map-1 x)))

        (define (map-n x-y)

          (define (map-n lsts)
            (let ((rests (##cdrs lsts)))
              (if (not rests)
                  '()
                  (if (pair? rests)
                      (let* ((args (##cars lsts '()))
                             (result (apply proc args))
                             (tail (map-n rests)))
                        (macro-if-checks
                         (if (fixnum? tail)
                             tail
                             (cons result tail))
                         (cons result tail)))
                      (macro-if-checks
                       rests
                       '())))))

          (macro-if-checks
           (let ((result (map-n x-y)))
             (if (fixnum? result)
                 (if (fx< result 0)
                     (macro-fail-check-list
                      (fx- (fx+ 2 result) ##min-fixnum)
                      (map proc . x-y))
                     (##raise-length-mismatch-exception
                      (fx+ 2 result)
                      '()
                      map
                      proc
                      x-y))
                 result))
           (map-n x-y)))

        (if (null? y)
            (map-1 x)
            (map-n (cons x y)))))))

(define-prim (##for-each proc x . y)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (define (for-each-1 x)

    (define (for-each-1 lst1)
      (if (pair? lst1)
          (begin
            (proc (car lst1))
            (for-each-1 (cdr lst1)))
          (void)))

    (for-each-1 x))

  (define (for-each-n x-y)

    (define (for-each-n lsts)
      (let ((rests (##cdrs lsts)))
        (if (not rests)
            (void)
            (if (pair? rests)
                (begin
                  (apply proc (##cars lsts '()))
                  (for-each-n rests))
                (void)))))

    (for-each-n x-y))

  (if (null? y)
      (for-each-1 x)
      (for-each-n (cons x y))))

(define-prim (for-each proc x . y)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
  (namespace ("" for-each)) ;; but not for-each to ##for-each

  (macro-force-vars (proc)
    (macro-check-procedure proc 1 (for-each proc x . y)
      (let ()

        (define (for-each-1 x)

          (define (for-each-1 lst1)
            (macro-force-vars (lst1)
              (if (pair? lst1)
                  (begin
                    (proc (car lst1))
                    (for-each-1 (cdr lst1)))
                  (macro-check-list lst1 2 (for-each proc x)
                    (void)))))

          (for-each-1 x))

        (define (for-each-n x-y)

          (define (for-each-n lsts)
            (let ((rests (##cdrs lsts)))
              (if (not rests)
                  (void)
                  (if (pair? rests)
                      (begin
                        (apply proc (##cars lsts '()))
                        (for-each-n rests))
                      (macro-if-checks
                       (if (fx< rests 0)
                           (macro-fail-check-list
                            (fx- (fx+ 2 rests) ##min-fixnum)
                            (for-each proc . x-y))
                           (##raise-length-mismatch-exception
                            (fx+ 2 rests)
                            '()
                            for-each
                            proc
                            x-y))
                       (void))))))

          (for-each-n x-y))

        (if (null? y)
            (for-each-1 x)
            (for-each-n (cons x y)))))))

;;;----------------------------------------------------------------------------
(define-prim (##cons*-aux x rest)

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

  (if (pair? rest)
      (let loop ((x x) (probe rest))
        (let ((y (car probe))
              (tail (cdr probe)))
          (set-car! probe x)
          (if (pair? tail)
              (loop y tail)
              (begin
                (set-cdr! probe y)
                rest))))
      x))

(define-prim (##cons* x . rest)
  (##cons*-aux x rest))

(define-prim (cons* x . rest)
  (##cons*-aux x rest))

 (define-prim (##make-list n #!optional (fill 0))

   (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc

   (let loop ((i n) (result '()))
     (if (fx> i 0)
         (loop (fx- i 1) (cons fill result))
         result)))

(define-prim (make-list n #!optional (fill (macro-absent-obj)))

  (include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
  (namespace ("" make-list)) ;; but not make-list to ##make-list

  (macro-force-vars (n fill)
    (macro-check-index n 1 (make-list n fill)
      (if (eq? fill (macro-absent-obj))
          (##make-list n)
          (##make-list n fill)))))
