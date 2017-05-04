#lang racket/base
(require "../string/convert.rkt"
         "check.rkt"
         "path.rkt"
         "sep.rkt")

(provide build-path
         build-path/convention-type)

(define (build-path base . subs)
  (build 'build-path #f base subs))

(define (build-path/convention-type convention base . subs)
  (build 'build-path/convention-type convention base subs))

(define (build who init-convention base subs)
  (check-build-path-arg who base)
  (define convention
    (let loop ([convention (argument->convention base init-convention who #:first? #t)]
               [subs subs])
      (cond
       [(null? subs) convention]
       [else
        (define sub (car subs))
        (check-build-path-arg who sub)
        (loop  (argument->convention sub convention who #:first? #f)
               (cdr subs))])))
  (path (append-path-parts convention who base subs)
        convention))

;; ----------------------------------------

(define (check-build-path-arg who p)
  (check who
         (lambda (p) (or (path-string? p)
                    (path-for-some-system? p)
                    (eq? p 'up)
                    (eq? p 'same)))
         #:contract "(or/c path-string? path-for-some-system? 'up 'same)"
         p))

(define (argument->convention p convention who #:first? first?)
  (define (check c)
    (when (and convention (not (eq? c convention)))
      (raise-arguments-error who
                             (format
                              (if first?
                                  "specified convention incompatible with ~a path element"
                                  "preceding path's convention incompatible with ~a path element")
                              (if (string? p)
                                  "string"
                                  "given"))
                             "path element" p
                             (if first? "convention" "preceding path's convention")
                             convention))
    c)
  (cond
   [(path? p) (check (path-convention p))]
   [(string? p) (check (system-path-convention-type))]
   [else convention]))

;; ----------------------------------------

(define (append-path-parts convention who base subs)
  (apply
   bytes-append
   (let loop ([prev (as-bytes base)] [subs subs])
     (cond
      [(null? subs) (list prev)]
      [else
       (define sub (car subs))
       (define bstr (as-bytes sub))
       (cond
        [(eq? convention 'unix)
         (when (is-sep? (bytes-ref bstr 0) 'unix)
           (raise-arguments-error who
                                  "absolute path cannot be added to a path"
                                  "absolute path" sub))
         (cons
          prev
          (cond
           [(is-sep? (bytes-ref prev (sub1 (bytes-length prev))) 'unix)
            (loop bstr (cdr subs))]
           [else
            (cons #"/" (loop bstr (cdr subs)))]))]
        [else
         ;; So much more to do here...
         (define cleaned-prev (strip-trailing-spaces prev))
         cleaned-prev])]))))

(define (as-bytes p)
  (cond
   [(eq? p 'up) #".."]
   [(eq? p 'same) #"."]
   [(path? p) (path-bytes p)]
   [else (string->bytes/locale p (char->integer #\?))]))

;; ----------------------------------------

;; FIXME
(define (strip-trailing-spaces prev)
  prev)
