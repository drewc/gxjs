(namespace (""))
(##declare
  (multilisp)
  (extended-bindings)
  (not safe)
  (block)
  (inlining-limit 134)
  ;(not run-time-bindings)
;  (standard-bindings)
)
(declare (extended-bindings))

;;; Common modules of the runtime system.
(##define-macro (incl filename)
  `(##declare-scope
    (##macro-scope
     (##namespace-scope
      (##include ,filename)))))

(incl "universal/prim.scm")
(incl "rts.scm")

(define (##init-gambit-module mod)
  (let ((init (##vector-ref mod 4)))
    (if (not (##procedure? init)) (##inline-host-statement "alert('Cannot find init function in ' + RTS.module_name(@1@)); " mod)
        (init))))

(define (##init-gambit-program)
  (declare (extended-bindings) (not safe))
    (let ((mods (##vector-ref ##program-descr 0)))
      (let loop ((i 1)) ;; start at module after the current one
        (if (##fx< i (##vector-length mods))
            (let ((mod (##vector-ref mods i)))
              (##init-gambit-module mod) ;; call module's init procedure
              (loop (##fx+ i 1)))))))


(##init-gambit-program)

;;; (##inline-host-declaration "console.log('Loaded univ.scm file')")

(##inline-host-statement #<<EOF
RTS.module_register = function(module_descr) {
  const r = this;
  r.sp = -1;
  r.stack[++this.sp] = void 0;
  r.r0 = this.underflow;
  r.nargs = 0;
  r.trampoline(module_descr[4]);
};

EOF
)
