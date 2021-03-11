namespace: js
package: #f
(import :js)

(##inline-host-declaration #<<EOF
RTS.GxJS === undefined ? RTS.GxJS = {} : null;

RTS.GxJS.scm2propName = (scm => {
  if (typeof scm === 'string' || typeof scm === 'number') {
    return scm
  } else if (scm instanceof RTS.Keyword || scm instanceof RTS.Symbol) {
    return scm.name
  } else {
    try {
      return RTS.scm2host(scm)
    } catch(e) {
      console.error('Cannot build property name from', scm)
      // return it anyway as non-strict js allows such silly keys
      return scm
    }
  }
})

RTS.GxJS.camelCase = (string) => {
    return string.replace( /-([a-z])/ig, function( all, letter ) {
        return letter.toUpperCase();
    });
};
RTS.GxJS.ref = (obj, ...refs) => {
  if (refs.length === 0) {return obj}
  if (obj instanceof RTS.Foreign) {
    obj = RTS.foreign2host(obj);
  } else if (obj === undefined) {
    return;
  };
  const ref = RTS.GxJS.scm2propName(refs[0])
  refs = refs.slice(1)
  const newObj = (o => {
    // If it is not here, perhaps we are trying.to-use-dashes
    // when.itShouldBeCamelCase?
    if (o === undefined) {
      const newRef = RTS.GxJS.camelCase(ref)
      // console.log('trying', newRef);
      return obj[newRef]
    } else { return o }
  })(obj[ref]);
  return RTS.GxJS.ref(newObj, ...refs)
};
RTS.GxJS.set_ref = (obj, ...refsAndValue) => {
  if (obj instanceof RTS.Foreign) {
    obj = RTS.foreign2host(obj);
  }

  if (refsAndValue.length === 2) {
    const ref =  RTS.GxJS.scm2propName(refsAndValue[0])
    const val = refsAndValue[1];
    obj[ref] = val;
    return val
  }
  const ref = RTS.GxJS.scm2propName(refsAndValue[0])
  const newObj = ( o => (o === undefined) ? {} : o )(obj[ref]);
  const newRefsAndValue = refsAndValue.slice(1)
  return RTS.GxJS.set_ref(newObj, ...newRefsAndValue)
};

RTS.GxJS.make_nary_predicate =  (op, zeroOrOne = true, recurse = false) => {
  const pred = (...args) => {
    if (args.length < 2) {
     return zeroOrOne;
    } else {
      const x = args[0], ys = args.slice(1);
      const res = ys.every(y => op(x,y));

      if (!res && !recurse) {
        return false
      } else if (!recurse) {
        return true
      } else {
         return RTS.GxJS.make_nary_predicate(op, zeroOrOne, recurse)(...ys);
      }
    }
  }

  return pred;
}

RTS.GxJS.apply_predicate = (op, argsList, zeroOrOne = true, recurse = false) => {
   const args = RTS.list2vector(argsList);
   const pred = RTS.GxJS.make_nary_predicate(op, zeroOrOne, recurse);
   return pred(...args);
}

RTS.GxJS.plist2jso = function (plist) {
  if (plist === null) { return {} };

  const jso = {}; let scms = [plist] ; const heap = [jso];

  function pair2prop (pair = plist) {
  if (pair === null) { return  };
    const car = pair.car, cdr = pair.cdr;
    // console.log('Trying scm2host for key:', car);
    const propName = (() => {
      if (typeof car === 'string') {
        return car
      } else if (car instanceof RTS.Keyword || car instanceof RTS.Symbol) {
        return car.name
      } else {
        try {
          return RTS.scm2host(car)
        } catch(e) { throw "Cannot make property name from key" }
      }
    })();

    const propValue = (() => {
      if (cdr === null) { return cdr; }
      const cadr = (() => (cdr instanceof RTS.Pair) ? cdr.car : cdr)();
      // Have we already made this scm into a properly value?

      const idx = scms.findIndex(o => o === cadr);

      let hostValue = (() => {

        if (typeof cadr === 'object' && idx > -1)  {
          return heap[idx]
        } else if (typeof cadr !== 'string' ){
          try {
            return  RTS.scm2host(cadr)
          } catch(e) { return cadr }
        } else { return cadr }
      })();

      if (idx === -1) { scms.push(cadr); heap.push[hostValue] }
      if (hostValue === undefined) { hostValue = cadr };
      return hostValue;
    })();

    jso[propName] = propValue;

    //  console.log('key', propName, 'value', propValue)
    if (cdr instanceof RTS.Pair && cdr.cdr instanceof RTS.Pair) {
      pair2prop(cdr.cdr)
    }


  }

  pair2prop();
  return jso;
}

EOF
)

(def (scm->js scm) (##inline-host-expression "RTS.scm2host(@1@);" scm))
(def (js->scm js) (##inline-host-expression "RTS.host2scm(@1@);" js))
(def (foreign->js scm) (##inline-host-expression "RTS.foreign2host(@1@);" scm))
(def (js->foreign js) (##inline-host-expression "RTS.host2foreign(@1@);" js))



(def (ref obj . keys)
  (##inline-host-expression "(ks => RTS.GxJS.ref(@1@, ...ks))(@2@)"
                            obj (##list->vector keys)))
(def (ref-set! obj . keys-and-val)
  (##inline-host-expression "(ks => RTS.GxJS.set_ref(@1@, ...ks))(@2@)"
                            obj (##list->vector keys-and-val)))
(def (jso-ref obj . keys)
  (js->scm (##apply ref obj keys)))

(def (jso-ref-set! obj . keys-and-val)
  (##inline-host-expression "(() => {
  const args = (@1@), obj = (@2@)
  const val = args[args.length - 1];
  args[args.length - 1] = RTS.scm2host(val);
  return RTS.host2scm(RTS.GxJS.set_ref(obj, ...args));
 })();" (##list->vector keys-and-val) obj))

(def (plist->jso plist)
  (##inline-host-expression "RTS.host2foreign(RTS.GxJS.plist2jso(@1@));"
                            plist))

(def (jso . keys-and-vals) (plist->jso keys-and-vals))
(def (jso? obj)
  (and (foreign? obj) (##inline-host-expression "(typeof (@1@).val === 'object');" obj)))

(def (=== . args)
  (js#expression "RTS.GxJS.apply_predicate((x,y) => x === y, @1@);" args))

(def (undefined? obj)
  (js#expression "(o => o === undefined)(@1@)" obj))
(def (null? obj)
  (js#expression "(o => o === null)(@1@)" obj))
