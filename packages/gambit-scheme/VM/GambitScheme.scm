(##include "VM.scm")

(define (##vm-primordial-loop)
  (let lp ()
    (define proc? (##inline-host-expression "@host2scm@(main_vm.runProcedure)"))
    (define runMe (if proc? (eval (string->symbol proc?)) #f))
    (if (procedure? runMe)
      (begin
        (##inline-host-statement "main_vm.runProcedure = false")
        (runMe)))
    (##heartbeat!)
    (##thread-sleep! 1)
    (lp)))

(##vm-primordial-loop)
