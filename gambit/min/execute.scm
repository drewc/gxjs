(##inline-host-declaration "console.log('startup file loaded');")


;(##inline-host-statement "alert('Startup file inited!!');console.log('asd', (@1@));" +)
(define (##init-gambit-program)
  (declare (extended-bindings) (not safe))

  (define (##init-gambit-module mod)
  (let ((init (##vector-ref mod 4)))
    (if (not (##procedure? init)) (##inline-host-statement "alert('Cannot find init function in ' + g_module_name(@1@)); " mod)
        (init))))
  ;(##inline-host-statement "console.error('prog',g_glo['##program-descr'] )")
    (let ((mods (##vector-ref ##program-descr 0)))
      (let loop ((i 1)) ;; start at module after the current one
        (if (##fx< i (##vector-length mods))
            (let ((mod (##vector-ref mods i)))
              (##init-gambit-module mod) ;; call module's init procedure
              (loop (##fx+ i 1)))))))

(##init-gambit-program)

(##startup-processor!)

;; when and unless
(##define-macro (when condition . body)
  `(if ,condition (begin ,@body) #!void))
(##define-macro (unless condition . body)
  `(if ,condition #!void (begin ,@body)))

(##inline-host-statement "console.log('made it here!', (@1@))" (lambda () 1))
