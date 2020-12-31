(##namespace (""

char->integer
char-alphabetic?
char-ci<=?
char-ci<?
char-ci=?
char-ci>=?
char-ci>?
char-downcase
char-foldcase
char-lower-case?
char-numeric?
char-upcase
char-upper-case?
char-whitespace?
char<=?
char<?
char=?
char>=?
char>?
char?
digit-value
integer->char
))

;(define-prim (##char->integer c) (##inline-host-expression "(@1@).code;" c))
(define-prim (char->integer c)
  (macro-force-vars (c)
    (macro-check-char c 1 (char->integer c)
      (##char->integer c))))

(define-prim (integer->char n) (##integer->char n))
