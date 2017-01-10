#lang racket/base
(require "config.rkt"
         "readtable.rkt"
         "parameter.rkt")

(provide char-delimiter?)

(define (char-delimiter? c config)
  (define rt (read-config-readtable config))
  (define dc (or (and rt
                      (hash-ref (readtable-delimiter-ht rt) c #f)) ; #f => default for `c`
                 c))
  (cond
   [(eq? dc 'no-delimit) #f]
   [(not (char? dc)) #t]
   [else
    (or (char-whitespace? dc)
        (char=? dc #\()
        (char=? dc #\))
        (char=? dc #\[)
        (char=? dc #\])
        (char=? dc #\{)
        (char=? dc #\})
        (char=? dc #\')
        (char=? dc #\`)
        (char=? dc #\,)
        (char=? dc #\;)
        (char=? dc #\")
        (and (char=? dc #\.)
             (check-parameter read-cdot config)))]))
