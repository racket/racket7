
(define gensym
  (case-lambda
   [() (chez:gensym)]
   [(s) (cond
         [(string? s) (chez:gensym s)]
         [(symbol? s) (chez:gensym (symbol->string s))]
         [else (raise-argument-error
                'gensym
                "(or/c symbol? string?)"
                s)])]))

(define/who (symbol-interned? s)
  (check who symbol? s)
  (not (gensym? s)))

(define/who (symbol-unreadable? s)
  (check who symbol? s)
  (and (getprop s 'racket-unreadable) #t))

(define/who (symbol->string s)
  (check who symbol? s)
  (string-copy (chez:symbol->string s)))

(define/who (string->uninterned-symbol str)
  (check who string? str)
  (gensym (string->immutable-string str)))

(define/who (string->unreadable-symbol str)
  (check who string? str)
  (let ([str (string->immutable-string str)]
        [sym (string->symbol str)])
    (or (getprop sym 'racket-unreadable)
        (let ([u-sym (gensym str)])
          (putprop sym 'racket-unreadable u-sym)
          u-sym))))

(define/who symbol<?
  (case-lambda
   [(a b)
    (check who symbol? a)
    (check who symbol? b)
    (string<? (symbol->string a)
              (symbol->string b))]
   [(a . as)
    (check who symbol? a)
    (let loop ([a a] [as as] [r #t])
      (cond
       [(null? as) r]
       [else
        (let ([b (car as)])
          (check who symbol? b)
          (loop b (cdr as) (and r (symbol<? a b))))]))]))
