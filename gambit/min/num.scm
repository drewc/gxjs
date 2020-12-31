(##include "prim.scm")
(##define-macro (define-js-prim-op sym js-op default)
  (let ((fn (##string-append "((xs) => {
    if (xs.length === 0) { const none = g_scm2host(@2@); return none };
    if (xs.length === 1) { xs = [g_scm2host(@2@), xs[0]] };

 return xs.reduce((a, b) => a " js-op " b)})(g_scm2host(@1@));")))
    `(define-prim (,sym . xs)
       (##inline-host-expression ,fn (##list->vector xs) ,default))))

(define-js-prim-op ##+ "+" 0) (define-prim + ##+)
(define-js-prim-op ##- "-" 0) (define-prim - ##-)
(define-js-prim-op ##* "*" 1) (define-prim * ##*)
(define-js-prim-op ##/ "/" 1) (define-prim / ##/)

(define-prim (##min . args)
  (##inline-host-expression "((ns) => Math.min(...ns))(@1@);" (##list->vector args)))
(define-prim min ##min)
