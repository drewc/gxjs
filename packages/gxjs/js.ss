namespace: #f
package: #f
(import
  (prefix-in (only-in <MOP> @method) @)
  (only-in :gerbil/gambit foreign?))

(export @method @@method foreign? js#this js#function
        js#expression js#expr
        js#declaration js#decl
        js#statement js#stmt

        js#++)

(defsyntax (@method stx)
  (syntax-case stx ()
    ((_ kv ...)
     #'(js#jso kv ...))))

(defsyntax (js#expression stx)
  (syntax-case stx ()
    ((_ str args ...) #'(##inline-host-expression str args ...))))
(defsyntax (js#expr stx)
  (syntax-case stx ()
    ((_ str args ...) #'(##inline-host-expression str args ...))))
(defsyntax (js#statement stx)
  (syntax-case stx ()
    ((_ str args ...) #'(##inline-host-statement str args ...))))
(defsyntax (js#stmt stx)
  (syntax-case stx ()
    ((_ str args ...) #'(##inline-host-statement str args ...))))
(defsyntax (js#declaration stx)
  (syntax-case stx ()
    ((_ str) #'(##inline-host-declaration str))))
(defsyntax (js#decl stx)
  (syntax-case stx ()
    ((_ str) #'(##inline-host-declaration str))))


(def js#this (##inline-host-expression "{};"))
(def js#arguments #())


(defsyntax (js#function stx)
  (syntax-case stx ()
    ((macro _args body ...)
     (let* ((args (syntax->datum #'_args))
            (binds (let lp ((bs args) (n 0))
                     (if (null? bs) bs
                         (cons (list (car bs) `(##vector-ref js#arguments ,n))
                               (lp (cdr bs) (+ n 1)))))))
       (with-syntax ((lbinds (datum->syntax #'macro binds))
                     (this
                      (datum->syntax #'macro 'js#this))
                     (args (datum->syntax #'macro 'js#arguments)))
           #'(let ((fn (lambda (this args)
                         (let lbinds
                             (begin (##inline-host-expression "undefined") body ...)))))
               (js#js->foreign (##inline-host-expression
                              "function (...args) {
  let scmProc = RTS.scm2host(@1@);
  const stack = (() => {
    const s = [];
    for (let key in RTS.stack) {
      s[key] = RTS.stack[key]
    }
    return s;
  })()
  const sp = RTS.sp

  // console.log('STACK Before scmProc(this, args):', RTS.sp,  RTS.stack);
  // console.log('our stack', sp , stack);

  const ret = scmProc(this, args);

  // console.log('STACK After Call:', RTS.sp, RTS.stack);
  // NOTE: this only seems to be when a bunch of FFI happens. Still, works both ways regardless.
   RTS.sp = sp;
  RTS.stack = stack;
  // console.log('STACK3', sp, RTS.sp, stack, RTS.stack, stack.length, RTS.stack.length);

  return ret;
};" (lambda (t a) (fn t a))))))))))



(defsyntax (js#++ stx)
  (syntax-case stx ()
    ((macro place number)
       #'(let ((val place))
           (set! place (+ val number))))
    ((macro place)
     #'(macro place 1))))
