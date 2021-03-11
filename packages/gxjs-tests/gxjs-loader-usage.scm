(declare (extended-bindings))

(##inline-host-declaration "console.log('Started Gambit-only GxJS loader!')")

(define property-name "This is valid")

(define GxJS-vector
  (##vector
   property-name 42
   'this "is how we hake the moonshine"))


(define (this-is-gambit-gxjs! #!optional (val 42))
  (##inline-host-statement "console.log('This is GxJS!', (@1@), (@2@))"
               val GxJS-vector))

(##inline-host-statement "console.log('finished Gambit-only GxJS loader');
 module.exports = RTS.scm2host(@1@);" this-is-gambit-gxjs!)
