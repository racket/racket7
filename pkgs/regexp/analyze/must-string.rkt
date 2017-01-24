#lang racket/base
(require "../parse/ast.rkt")

(provide get-must-string)

;; If there's something expensive in the regexp, look for a string
;; that must be in the input, which is useful as a pre-check for
;; matching.

(define (get-must-string rx)
  (and (something-expensive? rx)
       (must-string rx)))

(define (something-expensive? rx)
  (cond
   [(or (rx:alts? rx) (rx:repeat? rx)) #t]
   [(rx:maybe? rx)
    (something-expensive? (rx:maybe-rx rx))]
   [(rx:sequence? rx)
    (for/or ([rx (in-list (rx:sequence-rxs rx))])
      (something-expensive? rx))]
   [(rx:conditional? rx)
    (or (something-expensive? (rx:conditional-rx1 rx))
        (something-expensive? (rx:conditional-rx2 rx)))]
   [(rx:group? rx)
    (something-expensive? (rx:group-rx rx))]
   [(rx:cut? rx)
    (something-expensive? (rx:cut-rx rx))]
   [(rx:lookahead? rx)
    (something-expensive? (rx:lookahead-rx rx))]
   [(rx:lookbehind? rx)
    (something-expensive? (rx:lookbehind-rx rx))]
   [else #f]))

(define (must-string rx)
  (cond
   [(bytes? rx) rx]
   [(integer? rx) (bytes rx)]
   [(rx:sequence? rx)
    (for/fold ([bstr #f]) ([rx (in-list (rx:sequence-rxs rx))])
      (define bstr1 (must-string rx))
      (cond
       [(not bstr) bstr1]
       [(not bstr1) bstr]
       [((bytes-length bstr) . > . (bytes-length bstr1)) bstr]
       [else bstr1]))]
   [(rx:repeat? rx)
    (and (positive? (rx:repeat-min rx))
         (must-string (rx:repeat-rx rx)))]
   [(rx:group? rx)
    (must-string (rx:group-rx rx))]
   [(rx:cut? rx)
    (must-string (rx:cut-rx rx))]
   [(rx:lookahead? rx)
    (and (rx:lookahead-match? rx)
         (must-string (rx:lookahead-rx rx)))]
   [(rx:lookbehind? rx)
    (and (rx:lookbehind-match? rx)
         (must-string (rx:lookbehind-rx rx)))]
   [else #f]))
