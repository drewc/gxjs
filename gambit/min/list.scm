(##include "~~/lib/gambit/list/list.scm")

(define-prim (##append-reverse! lst tail)
  (let loop ((prev tail) (curr lst))
    (if (##pair? curr)
        (let ((next (##cdr curr)))
          (##set-cdr! curr prev)
          (loop curr next))
        prev)))
