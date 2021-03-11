(declare (extended-bindings))

(##inline-host-declaration "console.log('Started Gambit loaded file!')")


(define gambit-vector
  (##vector
   42 'this "is how we hake the moonshine"))


(define (this-is-gambit! #!optional (val 42))
  (let ((three (##inline-host-expression "{ answer: 42 };")))
    (##inline-host-statement "console.log('This is Gambit!', (@1@), (@2@), (@3@))"
                           val gambit-vector three)))

(##inline-host-statement "console.log('finished Gambit-loaded file');
 module.exports = RTS.scm2host(@1@);" this-is-gambit!)
