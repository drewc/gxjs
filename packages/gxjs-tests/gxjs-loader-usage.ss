(import :js)

(js#declaration "console.log('started GxJS loader!')")

(def property-name "This is valid")

(def GxJS-jso
  {
    thisIsTheAnswer: 42
    'this "is how we make the moonshine"
    property-name 'symbols-have-no-js-value!
  })


(def (this-is-gxjs! (val 42))
  (js#statement "console.log('This is GxJS!', (@1@), (@2@), (@3@))"
                val
                (js#foreign->js GxJS-jso)
                (js#scm->js (string-append "string " "append"))))

(js#statement "console.log('finished GxJS loader');
 module.exports = RTS.scm2host(@1@);" this-is-gxjs!)
