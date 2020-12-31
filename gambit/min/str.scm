(define-prim (##string->vector str)
  (##inline-host-expression "(@1@).codes.map((c) => new G_Char(c));" str))
(define-prim string->vector ##string->vector)
(define-prim (##vector->string vec)
  (##inline-host-expression "new G_ScmString((@1@).map((char) => char.code))" vec))
(define-prim vector->string ##vector->string)
(define-prim (##list->string lst) (##vector->string (##list->vector lst)))
(define-prim list->string ##list->string)
(define-prim (##string->list str) (##vector->list (##string->vector str)))
(define-prim string->list ##string->list)
(define-prim (##make-string k #!optional (fill 0)) (##make-string k fill))
(define-prim make-string ##make-string)

(define-prim (##string . chars) (##list->string chars))
(define-prim string ##string)


(define-prim (##string-append . strs)
  (##vector->string (apply ##vector-append (map ##string->vector strs))))
(define-prim string-append ##string-append)

(define-prim (##string-copy str
                            #!optional
                            (start undefined)
                            (end undefined))
  (##inline-host-expression "(() => {
   const vec = (@1@).codes ; return new G_ScmString(vec.slice(@2@, @3@));
 })();" str start end))
(define-prim string-copy ##string-copy)
(define-prim ##substring ##string-copy)
(define-prim substring ##string-copy)

(define (##str->vec str) (##inline-host-expression "(@1@).codes" str))

(define-prim (##string-copy! strto at strfrom
                               #!optional
                               (start undefined)
                               (end undefined))
  1
  (let ((vec (##str->vec strto))
        (from (##str->vec strfrom)))
    (##vector-copy! vec at from start end)
    strto))

(define-prim string-copy! ##string-copy!)

(define-prim (##string-fill! str charfill
                             #!optional
                             (start 0)
                             (end #f))

  (let ((vec (##str->vec str))
        (fill (##inline-host-expression "(@1@).code" charfill)))
   ;; (##inline-host-statement "console.error(@1@, @2@);" str vec)
    (##vector-fill! vec fill start end)
   ;; (##inline-host-statement "console.error(@1@, @2@);" str vec)
    str))
(define-prim string-fill! ##string-fill!)

(define-prim (##string-length str)
  (##inline-host-expression "(@1@).codes.length"))
(define-prim string-length ##string-length)

(define-prim (##string-for-each fn . strs)
  (let ((end (apply ##min (map ##string-length strs))))
    ;;(##inline-host-statement "console.log('end', (@1@))" end)
    (letrec ((sfe (lambda (n)
                    (unless (= n end)
                      (apply fn (map (lambda (s) (##string-ref s n)) strs))
                      (sfe (+ 1 n))))))
      (sfe 0))))

(define-prim string-for-each ##string-for-each)

(define-prim (##string<=? . strs) (apply ##<= (map ##str->vec strs)))
(define-prim string<=? ##string<=?)
(define-prim (##string<? . strs) (apply ##< (map ##str->vec strs)))
(define-prim string<? ##string<?)
(define-prim (##string>=? . strs) (apply ##>= (map ##str->vec strs)))
(define-prim string>=? ##string>=?)
(define-prim (##string>? . strs) (apply ##> (map ##str->vec strs)))
(define-prim string>? ##string>?)

(define-prim (##string-equal? . strs)
  (##inline-host-expression "console.error(@1@)" strs)
  (apply ##vector-equal? (map ##string->vector strs)))

(define-prim string-equal? ##string-equal?)
