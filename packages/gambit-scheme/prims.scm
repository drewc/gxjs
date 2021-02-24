(declare (extended-bindings))
(namespace (""))

(define-prim (foreign? obj) (##foreign? obj))

;;; (##inline-host-declaration "console.log('Success declare for prims.scm!!')")
;;; (##inline-host-statement "console.log('Success statement for prims.scm!!')")
