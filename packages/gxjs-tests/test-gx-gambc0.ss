(def (test> name i pred j)
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

(##inline-host-declaration "console.log('loading gx-gambc0-tests')")

;; (namespace ("") %%apply)
(extern namespace: #f %%apply)
(def foo (%%apply list '(4 2)))

(def (test-gambc0)
  (test> "Gambc0 needs gx-gambc#" (car (##apply ##list '(1 2))) ##fx= 1)
  )
