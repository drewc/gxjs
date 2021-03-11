(def (test> name i pred j)
  (let ((res (pred i j)))
    (##inline-host-statement "
     (() => {
      function hst (thing) { return thing
         // try { return RTS.scm2host(thing) } catch { return thing } ;
     };
     const name = hst(@1@);
     const i = hst(@2@);
     const j = hst(@3@);
     const res = !!(@4@);
     const msg = name + ' ' + JSON.stringify(i) + ' => ' + JSON.stringify(j);
     if (res) {
       console.log('Success:', msg)
     } else {
       console.error('Failure', msg)
     }
   })()

" name i j res)))

(##inline-host-declaration "console.log('loading gx-gambjs ests')")
(##inline-host-statement "console.log('loaded gx-gamjs testes')")

(declare (extended-bindings))
(extern namespace: #f
  ;;; shims to ensure inlining of low level primitives
  %%apply

;; when and unless


  )

(def whereami #t)


(def (bar) (if whereami 42 43))

(def (test-when)
  (let ((n (when whereami 2)))
    (##inline-host-statement "console.log('WTF Gambc?', (@1@))" n)
    n))

(def (test-unless) (unless test-when 42))

(def (test-gx-gambc#)
  (test> "%%apply" (car (##apply list '(1 2))) ##fx= 1)
  ;;(test> "%%apply" (list whereami 1) ##fx= 1)
  (test> "when" (test-when) ##fx= 2)
  (test> "unless" (test-unless) eq? (void))


  42)




(def (test-base-atoms)
  (test> "void?" (void? (void)) eq? #t))


(def (ok-hash!)
  (let ((eq-ht (make-hash-table-eq)))
    (hash-table? eq-ht))
  )

(def (test-hash-table)
  (let ((eq-ht (make-hash-table-eq)))
    (##inline-host-statement "console.log('testing hash tables', (@1@))" (ok-hash!))
    (test> "hash-table?" (hash-table? eq-ht) eq? #t)))



(def (test-keyword-dispatch)
  (def (kwfn table . args)
    (test> "kw hash?" (hash-table? table) eq? #t)
    (test> "kw args" (car args) ##fx= 42)
    (hash-ref table dispatch:))
  (let ((kw (keyword-dispatch #f kwfn dispatch: 'yup 42 1 2 3)))
    (test> "dispatch" kw eq? 'yup)))

(def (test-gx-gambc)
  (test-gx-gambc#)
  (test-base-atoms)
  (test-hash-table)
  (test-keyword-dispatch))

(##inline-host-statement "module.exports = RTS.scm2host(@1@)" test-gx-gambc)
