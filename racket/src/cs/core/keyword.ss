
(define-record keyword (symbol))

(define (string->keyword s)
  (define sym (string->symbol s))
  (or (getprop sym 'keyword #f)
      (let ([kw (make-keyword sym)])
        (putprop sym 'keyword kw)
        kw)))

(define (keyword->string kw)
  (symbol->string (keyword-symbol kw)))

(define (keyword<? a b)
  (symbol<? (keyword-symbol a)
            (keyword-symbol b)))

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))
