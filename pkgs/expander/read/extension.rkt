#lang racket/base
(require "config.rkt"
         "consume.rkt"
         "error.rkt"
         "accum-string.rkt"
         "parameter.rkt"
         "wrap.rkt")

(provide read-extension-reader
         read-extension-lang
         read-extension-#!)

(define (read-extension-reader read-one read-recur dispatch-c in config)
  (define extend-str (read-extension-prefix (cons dispatch-c '(#\r #\e))
                                            '(#\a #\d #\e #\r)
                                            in
                                            config))
  (unless (check-parameter read-accept-reader config)
    (reader-error in config
                  "`~a` not enabled"
                  extend-str))

  (define mod-path-datum (read-one in (disable-wrapping config)))
  (when (eof-object? mod-path-datum)
    (reader-error in config
                  "expected a datum after `~a`, found end-of-file"
                  extend-str))
  
  (read-extension mod-path-datum read-recur in config))

;; ----------------------------------------

(define (read-extension-lang read-recur dispatch-c in config)
  (define extend-str (read-extension-prefix (cons dispatch-c '(#\l))
                                            '(#\a #\n #\g)
                                            in
                                            config))
  (define c (read-char-or-special in))
  (unless (char=? c #\space)
    (reader-error in config
                  "expected a single space after `~a`"
                  extend-str))
  
  (read-lang extend-str read-recur in config))

(define (read-extension-#! read-recur dispatch-c in config)
  (define c (read-char-or-special in))
  (unless (char-lang-nonsep? c)
    (bad-syntax-error in config (if (char? c)
                                    (string dispatch-c #\! c)
                                    (string dispatch-c #\!))))
  (read-lang (string dispatch-c #\!) read-recur in config
             #:init-c c))

;; ----------------------------------------

(define (read-lang extend-str read-recur in config
                   #:init-c [init-c #f])
  (unless (and (check-parameter read-accept-reader config)
               (check-parameter read-accept-lang config))
    (reader-error in config
                  "`~a` not enabled"
                  extend-str))
  
  (define accum-str (accum-string-init! config))
  (when init-c
    (accum-string-add! accum-str init-c))
  (let loop ()
    (define c (read-char-or-special in))
    (cond
     [(eof-object? c) (void)]
     [(not (char? c))
      (reader-error in config
                    "found non-character while reading `#~a'"
                    extend-str)]
     [(char-whitespace? c) (void)]
     [(or (char-lang-nonsep? c)
          (char=? #\/ c))
      (accum-string-add! accum-str c)
      (loop)]
     [else (reader-error in config
                         (string-append "expected only alphanumeric, `-', `+', `_', or `/'"
                                        " characters for `~a', found `~a'")
                         extend-str
                         c)]))

  (define lang-str (accum-string-get! accum-str config))
  (when (equal? lang-str "")
    (reader-error in config
                  "expected a non-empty sequence of alphanumeric, `-', `+', `_', or `/' after `~a'"
                  extend-str))

  (when (char=? #\/ (string-ref lang-str 0))
    (reader-error in config
                  "expected a name that does not start `/' after `~a'"
                  extend-str))

  (when (char=? #\/ (string-ref lang-str (sub1 (string-length lang-str))))
    (reader-error in config
                  "expected a name that does not end `/' after `~a'"
                  extend-str))
  
  (define submod-path `(submod ,(string->symbol lang-str) reader))
  (define reader-path (string->symbol (string-append lang-str "/lang/reader")))
  
  (read-extension #:try-path-first submod-path
                  reader-path read-recur in config))

(define (char-lang-nonsep? c)
  (and ((char->integer c) . < . 128)
       (or (char-alphabetic? c)
           (char-numeric? c)
           (char=? #\- c)
           (char=? #\+ c)
           (char=? #\_ c))))

;; ----------------------------------------

(define (read-extension-prefix already wanted in config)
  (define accum-str (accum-string-init! config))
  (for ([c (in-list already)])
    (accum-string-add! accum-str c))
  (let loop ([wanted wanted])
    (unless (null? wanted)
      (define c (read-char-or-special in))
      (when (char? c)
        (accum-string-add! accum-str c))
      (unless (eqv? c (car wanted))
        (bad-syntax-error in config (accum-string-get! accum-str config)))
      (loop (cdr wanted))))
  (accum-string-get! accum-str config))

;; ----------------------------------------

(define (read-extension #:try-path-first [submod-path #f]
                        mod-path-datum read-recur in config)
  (define mod-path ((check-parameter current-reader-guard config)
                    mod-path-datum))
  (define for-syntax? (read-config-for-syntax? config))
  
  (define extension
    ((read-config-dynamic-require config)
     mod-path
     (if for-syntax? 'read-syntax 'read)
     #f))
  
  (define result-v
    (cond
     [for-syntax?
      (cond
       [(procedure-arity-includes? extension 6)
        (extension mod-path-datum 
                   in
                   (read-config-source config)
                   (read-config-line config)
                   (read-config-col config)
                   (read-config-pos config))]
       [(procedure-arity-includes? extension 2)
        (extension mod-path-datum in)]
       [else
        (raise-argument-error '|#reader|
                              "(or/c (procedure-arity-includes?/c 2) (procedure-arity-includes?/c 6))"
                              extension)])]
     [else
      (cond
       [(procedure-arity-includes? extension 5)
        (extension in
                   (read-config-source config)
                   (read-config-line config)
                   (read-config-col config)
                   (read-config-pos config))]
       [(procedure-arity-includes? extension 1)
        (extension in)]
       [else
        (raise-argument-error '|#reader|
                              "(or/c (procedure-arity-includes?/c 1) (procedure-arity-includes?/c 5))"
                              extension)])]))
  
  (cond
   [(special-comment? result-v)
    (read-recur in config)]
   [else
    result-v]))
