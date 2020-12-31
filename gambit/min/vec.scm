(define-prim (##vector->list vec)
  (##inline-host-expression "((vec) => {
     function vec2list(n = 0) {
      if (vec.length === n) { return null; }
       return new G_Pair(vec[n], vec2list(n + 1));
     };
     return vec2list();
})(@1@)" vec))

(define-prim vector->list ##vector->list)

(define-prim (##make-vector k #!optional (fill 0))
  (##make-vector k fill))

(define-prim make-vector ##make-vector)

#;(define-prim (##make-s8vector k #!optional (fill 0))
  (##make-s8vector k fill))

(define-prim (##make-u8vector k #!optional (fill 0))
  (##make-u8vector k fill))

(define-prim make-u8vector ##make-u8vector)
#;(define-prim (##make-s16vector k #!optional (fill 0))
  (##make-s16vector k fill))

#;(define-prim (##make-u16vector k #!optional (fill 0))
  (##make-u16vector k fill))

#;(define-prim (##make-s32vector k #!optional (fill 0))
  (##make-s32vector k fill))

#;(define-prim (##make-u32vector k #!optional (fill 0))
  (##make-u32vector k fill))

#;(define-prim (##make-s64vector k #!optional (fill 0))
  (##make-s64vector k fill))

#;(define-prim (##make-u64vector k #!optional (fill 0))
  (##make-u64vector k fill))

#;(define-prim (##make-f32vector k #!optional (fill 0.0))
  (##make-f32vector k fill))

#;(define-prim (##make-f64vector k #!optional (fill 0.0))
  (##make-f64vector k fill))

(define-prim (##list->vector lst)
 (##inline-host-expression "
((list) => {
  function list2vector(lst, vec = []) {
    if (lst === null) { return vec };
    vec.push(lst.car);
    return list2vector(lst.cdr, vec)
   };
    return list2vector(list);
})(@1@);" lst))
(define-prim list->vector ##list->vector)

(define-prim (##vector . xs) (##list->vector xs))
(define-prim (##vector-ref vec ref)
  (##inline-host-expression "(@1@)[@2@];" vec ref))
(define-prim (##vector-set! vec ref val)
  (##inline-host-expression "(@1@)[@2@] = (@3@);" vec ref val))

(define-prim (##vector-append . xs)
  (##inline-host-expression "Array.prototype.concat.apply([], (@1@));"
                            (##list->vector xs)))
(define-prim vector-append ##vector-append)

(define undefined (##inline-host-expression "undefined"))

(define-prim (##vector-copy vec
                            #!optional
                            (start undefined)
                            (end undefined))
  (##inline-host-expression "(@1@).slice(@2@, @3@)" vec start end))
(define-prim vector-copy ##vector-copy)

;; (vector-copy! to at from) procedure
;; (vector-copy! to at from start) procedure
;; (vector-copy! to at from start end) procedure

(define-prim (##vector-copy! to at from
                             #!optional
                             (start 0)
                             (end #f))
  (let ((cp (##vector-copy from start end)))
    (##inline-host-expression "(() => {
     var at = (@1@); const to = (@2@);
     const from = (@3@); const start = (@4@); const endp = (@5@);
     const end = endp ? endp : from.length
      for (i = start ; i < end ; i++) {
        to[at] = from[i]; at = at + 1;
      }
     return to;
    })();" at to from start end)))

(define-prim vector-copy! ##vector-copy!)

;; (vector-fill! vector fill) procedure
;; (vector-fill! vector fill start) procedure
;; (vector-fill! vector fill start end) procedure
;; The vector-fill! procedure stores fill in the elements of
;; vector between start and end

(define-prim (##vector-fill! vec fill
                             #!optional
                             (start 0)
                             (end #f))
    (##inline-host-expression "((vec, fill) => {
       const endp = (@2@);
       const end = endp ? endp : vec.length;
       for (i = (@1@) ; i < end ; i++) {
         vec[i] = fill
      };
     return vec;
    })(@3@, @4@);" start end vec fill))

(define-prim vector-fill! ##vector-fill!)

(define-prim (##vector-map fn . vecs)
 (##list->vector (##apply map (cons fn (map ##vector->list vecs)))))

(define-prim vector-map ##vector-map)

(##define-macro (vec-maker maker vec)
  (let ((ihe (##string-append
              "(() => { var vec = (@1@) ; var bv = "maker"(); bv.elems = vec; return bv;})()")))
    `(##inline-host-expression ,ihe ,vec)))

(define (typed-vec->vec x) (##inline-host-expression "(@1@).elems" x))

(##define-macro (define-vec-construct name other-name maker)
  `(begin (define-prim (,name . xs) (vec-maker  "g_make_u8vector"(##list->vector xs)))
          (define-prim ,other-name ,name)))

(define-vec-construct ##u8vector u8vector "g_make_u8vector")
(define-prim (##u8vector? t) (##inline-host-expression "((@1@) instanceof G_U8Vector);" t))
(define-prim u8vector? ##u8vector?)

(define-prim (##u8vector-ref v r) (##vector-ref (typed-vec->vec v) r))
(define-prim (##u8vector-set! v r val) (##vector-set! (typed-vec->vec v) r val))
(define-prim u8vector-ref ##u8vector-ref)
(define-prim u8vector-set! ##u8vector-set!)

(define-prim (##u8vector-append . xs)
  (let* ((vs (map typed-vec->vec xs))
         (elems (apply ##vector-append vs)))
    (vec-maker "g_make_u8vector" elems)))
(define-prim u8vector-append ##u8vector-append)
(define bytevector-append  u8vector-append)

(define-prim (##u8vector-copy u8vec
                              #!optional
                              (start undefined)
                              (end undefined))
  (let* ((vec (typed-vec->vec u8vec))
         (cp (##vector-copy vec start end)))
    (vec-maker "g_make_u8vector" vec)))

(define-prim u8vector-copy ##u8vector-copy)


(define-prim (##u8vector-copy! u8vec at u8from
                               #!optional
                               (start undefined)
                               (end undefined))
  1
  (let ((vec (typed-vec->vec u8vec))
        (from (typed-vec->vec u8from)))
    (##vector-copy! vec at from start end)
    u8vec))

(define-prim u8vector-copy! ##u8vector-copy!)
(define bytevector-copy! u8vector-copy!)

(define-prim (##u8vector-length v) (##vector-length (typed-vec->vec v)))
(define-prim u8vector-length ##u8vector-length)

(##inline-host-declaration "lodashLang = require('lodash/lang');")
(define-prim (##vector-equal? . vs)
  (##inline-host-expression"((arrays) => {
     pred = (v) => { return lodashLang.isEqual(v, arrays[0]) };
     // console.error('ok! vecotr-erq', pred(arrays[1]), arrays)
     return arrays.every(pred);
})(@1@)" (##list->vector vs)))

(define-prim (##u8vector-equal? . vs)
  (##inline-host-expression"((vs) => {
     const arrays = vs.map((v) => v.elems);
     pred = (v) => { return lodashLang.isEqual(v, arrays[0]) };
     // console.error('ok!', pred(arrays[1]), arrays, vs)
     return arrays.every(pred);
})(@1@)" (##list->vector vs)))


(define bytevector?        u8vector?)
(define make-bytevector    make-u8vector)
(define bytevector         u8vector)
(define bytevector-length  u8vector-length)
(define bytevector-u8-ref  u8vector-ref)
(define bytevector-u8-set! u8vector-set!)
(define bytevector-copy    u8vector-copy)
(define bytevector-copy!   u8vector-copy!)
(define bytevector-append  u8vector-append)
