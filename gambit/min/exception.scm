(##include "~~/lib/_thread#.scm")
(define ##current-exception-handler
  (##make-parameter
   (lambda (exc)
     (##thread-end-with-uncaught-exception! exc))
   (lambda (val)
     (macro-check-procedure val 1 (##current-exception-handler val)
       val))))

(define current-exception-handler
  ##current-exception-handler)

;;;----------------------------------------------------------------------------

;;; User accessible primitives for exception handling.

(define-prim (##with-exception-handler handler thunk)
  (macro-dynamic-bind exception-handler
   handler
   thunk))

(define-prim (with-exception-handler handler thunk)
  (macro-force-vars (handler thunk)
    (macro-check-procedure handler 1 (with-exception-handler handler thunk)
      (macro-check-procedure thunk 2 (with-exception-handler handler thunk)
        (macro-dynamic-bind exception-handler
         handler
         thunk)))))

(define-prim (##with-exception-catcher catcher thunk)
  (##continuation-capture
   (lambda (cont)
     (macro-dynamic-bind exception-handler
      (lambda (exc)
        (##continuation-graft cont catcher exc))
      thunk))))

(define-prim (with-exception-catcher catcher thunk)
  (macro-force-vars (catcher thunk)
    (macro-check-procedure catcher 1 (with-exception-catcher catcher thunk)
      (macro-check-procedure thunk 2 (with-exception-catcher catcher thunk)
        (##with-exception-catcher catcher thunk)))))

(define-prim (##raise obj);;;;;;;;;;;;;;;;;;
  (macro-raise obj))

(define-prim (raise obj)
  (macro-raise obj))

(define-prim (##abort obj);;;;;;;;;;;;;;;;;;;;;;;;;;
  (macro-abort obj))

(define-prim (abort obj)
  (macro-abort obj))

(define (##r7rs-with-exception-handler handler thunk)
  (let ((original-eh (macro-current-exception-handler)))
    (macro-dynamic-bind
     exception-handler
     (lambda (exc)
       (macro-dynamic-bind
        exception-handler
        original-eh
        (lambda ()
          (handler exc))))
     thunk)))

(define (##r7rs-with-exception-catcher handler thunk)
  (##continuation-capture
   (lambda (cont)
     (##r7rs-with-exception-handler
      (lambda (exc)
        (##continuation-graft cont handler exc))
      thunk))))

(define (##r7rs-raise exc) ;; raise noncontinuable exception
  (##abort exc))

(define (##r7rs-raise-continuable exc) ;; raise continuable exception
  (##raise exc))

;;;----------------------------------------------------------------------------
