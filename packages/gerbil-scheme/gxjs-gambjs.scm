;;; -*- Gerbil -*-
;;; (C) me at drewc.ca
;;; (C) vyzo at hackzen.org

;;; Gerbil stage0 -- Gambit-JS host runtime

(##namespace (""))
;;(include "gx-gambc#.scm")

(declare
  (block)
  (standard-bindings)
  (extended-bindings))

(define absent-obj
  (macro-absent-obj))

(define absent-value
  '#(#!void))

(define (true . _)
  #t)
(define (true? obj)
  (eq? obj #t))

(define (false . _)
  #f)

(define (void . _)
  #!void)
(define (void? obj)
  (eq? obj #!void))

(define (eof-object . _)
  '#!eof)

(define (identity obj)
  obj)

(define make-hash-table make-table)
(define (make-hash-table-eq . args)
  (apply make-table test: eq? args))
(define (make-hash-table-eqv . args)
  (apply make-table test: eqv? args))

(define list->hash-table list->table)
(define (list->hash-table-eq lst . args)
  (apply list->table lst test: eq? args))
(define (list->hash-table-eqv lst . args)
  (apply list->table lst test: eqv? args))

(define hash?
  table?)
(define hash-table?
  table?)

(define hash-length
  table-length)
(define hash-ref
  table-ref)
(define (hash-get ht k)
  (table-ref ht k #f))
(define (hash-put! ht k v)
  (table-set! ht k v))
(define (hash-update! ht k update #!optional (default #!void))
  (let ((value (hash-ref ht k default)))
    (hash-put! ht k (update value))))

(define (hash-remove! ht k)
  (table-set! ht k))

(define hash->list
  table->list)

(define (hash->plist ht)
  (hash-fold cons* '() ht))

(define (plist->hash-table plst #!optional (ht (make-hash-table)))
  (let lp ((rest plst))
    (core-match rest
      ((k v . rest)
       (hash-put! ht k v)
       (lp rest))
      (() ht))))

(define (plist->hash-table-eq plst)
  (plist->hash-table plst (make-hash-table-eq)))
(define (plist->hash-table-eqv plst)
  (plist->hash-table plst (make-hash-table-eqv)))

(define (hash-key? ht k)
  (not (eq? (hash-ref ht k absent-value) absent-value)))

(define hash-for-each
  table-for-each)

(define (hash-map fun ht)
  (hash-fold
   (lambda (k v r) (cons (fun k v) r))
   '() ht))

(define (hash-fold fun iv ht)
  (let ((ret iv))
    (hash-for-each
     (lambda (k v) (set! ret (fun k v ret)))
     ht)
    ret))

(define hash-find
  table-search)

(define (hash-keys ht)
  (hash-map (lambda (k v) k) ht))

(define (hash-values ht)
  (hash-map (lambda (k v) v) ht))

(define (hash-copy hd . rest)
  (let ((hd (table-copy hd)))
    (if (null? rest) hd
        (apply hash-copy! hd rest))))

(define (hash-copy! hd . rest)
  (for-each (lambda (r) (table-merge! hd r)) rest)
  hd)

(define (hash-merge hd . rest)
  (foldl (lambda (tab r) (table-merge r tab))
         hd rest))

(define (hash-merge! hd . rest)
  (foldl (lambda (tab r) (table-merge! r tab))
         hd rest))

(define (hash-clear! ht #!optional (size 0))
  (let ((gcht (%%vector-ref ht 5)))
    (if (not (fixnum? gcht))
      (%%vector-set! ht 5 size))))


;; kwt: #f or a vector as a perfect hash-table for expected keywords
(define (keyword-dispatch kwt K . all-args)
  (when kwt
    (unless (vector? kwt)
      (##raise-type-exception 1 'vector 'keyword-dispatch
                              (cons* kwt K all-args))))
  (unless (procedure? K)
    (##raise-type-exception 2 'procedure 'keyword-dispatch
                            (cons* kwt K all-args)))
  (let ((keys (make-hash-table-eq hash: keyword-hash)))
    (let lp ((rest all-args) (args #f) (tail #f))
      (core-match rest
        ((hd . hd-rest)
         (cond
          ((keyword? hd)
           (core-match hd-rest
             ((val . rest)
              (when kwt
                (let ((pos (%%fxmodulo (keyword-hash hd) (%%vector-length kwt))))
                  (unless (eq? hd (%%vector-ref kwt pos))
                    (error "Unexpected keyword argument" K hd))))
              (when (hash-key? keys hd)
                (error "Duplicate keyword argument" K hd))
              (hash-put! keys hd val)
              (lp rest args tail))))
          ((eq? hd #!key)               ; keyword escape
           (core-match hd-rest
             ((val . rest)
              (if args
                (begin
                  (%%set-cdr! tail hd-rest)
                  (lp rest args hd-rest))
                (lp rest hd-rest hd-rest)))))
          ((eq? hd #!rest)              ; end keyword processing
           (if args
             (begin
               (%%set-cdr! tail hd-rest)
               (%%apply K (cons keys args)))
             (%%apply K (cons keys hd-rest))))
          (else                         ; plain argument
           (if args
             (begin
               (%%set-cdr! tail rest)
               (lp hd-rest args rest))
             (lp hd-rest rest rest)))))
        (else
         (if args
           (begin
             (%%set-cdr! tail '())
             (%%apply K (cons keys args)))
           (K keys)))))))


;;(##inline-host-statement "console.log('gambjs-runtime')")
