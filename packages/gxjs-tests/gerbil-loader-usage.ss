package: foo

(##inline-host-declaration "console.log('started gerbil loader!')")
(def (this-is-gerbil!)
  (##inline-host-statement "console.log('This is Gerbil!', (@1@))"
                           (string-append "foo" "bar")))

;; (error for incompelete form

(##inline-host-statement "console.log('finished gerbil loader');
 module.exports = RTS.scm2host(@1@);" this-is-gerbil!)
