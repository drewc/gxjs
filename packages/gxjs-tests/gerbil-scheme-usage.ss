(import :js)

(js#declaration "console.log('started gerbil scheme!')")

(def (this-is-gerbil!! (val 42))
  (js#statement "console.log('This is GxJS!', (@1@), (@2@))"
                val
                (js#expression "{ foo: 'bar' };")))

(js#statement "console.log('finished gerbil scheme init');
 module.exports = RTS.scm2host(@1@);" this-is-gerbil!!)
