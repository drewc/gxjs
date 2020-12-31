(namespace (""))
(##declare
  (multilisp)
  (extended-bindings)
  (not safe)
  (block)
  (inlining-limit 134)
  (not run-time-bindings)
)



;;;----------------------------------------------------------------------------

;;; Apply.

(define (##apply proc arg1 . rest)
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

;;;----------------------------------------------------------------------------
(##define-macro (##raise-range-exception . args)
  `(##inline-host-expression "(() => {
      const _err = (@1@);
      const _num = _err.car; console.error('range execption:', _num, 'is out of', _err);
  })()" '(,@args)))
(##define-macro (##raise-type-exception . args)
  `(##inline-host-expression "console.error('type execption:', @1@);" '(,@args)))
;;;----------------------------------------------------------------------------



;; when and unless
(##define-macro (when condition . body)
  `(if ,condition (begin ,@body) #!void))
(##define-macro (unless condition . body)
  `(if ,condition #!void (begin ,@body)))

;;; Common modules of the runtime system.
(##define-macro (incl filename)
  `(##declare-scope
    (##macro-scope
     (##namespace-scope
      (##include ,filename)))))


(incl "min/equality.scm")
;;(incl "min/cte.scm")
;;(incl "min/eval.scm")
(incl "min/list.scm")
;; (incl "min/error.scm")
;;(incl "min/vector.scm")
(incl "min/vec.scm")
(incl "min/structure.scm")
(incl "min/symkey.scm")
(incl "min/char.scm")
(incl "min/string.scm")
(incl "min/str.scm")
(incl "min/num.scm")
(incl "min/nums.scm")
(incl "min/num/qrm.scm")
;(incl "min/num/gcdlcm.scm")
(incl "min/thread.scm")
(incl "min/values.scm")
(incl "min/exception.scm")
(incl "min/module-registry.scm")
(incl "min/host2scm2host.scm")
(##inline-host-declaration
 "g_module_registry_init([new G_ModLinkInfo(\"gxjs-execute\",0),new G_ModLinkInfo(\"gxjs-minlib\",1)]);")
#;(##inline-host-statement "console.error(@1@)"
                         (##with-exception-handler (lambda _ 42) (lambda _ (raise 'an-error))))
