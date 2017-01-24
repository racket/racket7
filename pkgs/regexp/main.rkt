#lang racket/base
(require "match/regexp.rkt"
         "match/main.rkt"
         "replace/main.rkt")

(provide regexp
         byte-regexp
         pregexp
         byte-pregexp
         
         regexp-match
         regexp-match/end
         regexp-match-positions
         regexp-match-positions/end
         regexp-match?
         regexp-match-peek
         regexp-match-peek-positions
         regexp-match-peek-positions/end
         regexp-match-peek-immediate
         regexp-match-peek-positions-immediate
         regexp-match-peek-positions-immediate/end
         regexp-replace
         regexp-replace*
         
         regexp?
         byte-regexp?
         pregexp?
         byte-pregexp
         
         regexp-max-lookbehind)

(define (regexp p [handler #f])
  (unless (string? p) (raise-argument-error 'regexp "string?" p))
  (make-regexp 'regexp p #f #f handler))
  
(define (byte-regexp p [handler #f])
  (unless (bytes? p) (raise-argument-error 'byte-regexp "bytes?" p))
  (make-regexp 'byte-regexp p #f #t handler))

(define (pregexp p [handler #f])
  (unless (string? p) (raise-argument-error 'pregexp "string?" p))
  (make-regexp 'pregexp p #t #f handler))
  
(define (byte-pregexp p [handler #f])
  (unless (bytes? p) (raise-argument-error 'byte-pregexp "bytes?" p))
  (make-regexp 'byte-pregexp p #t #t handler))

(define (regexp-max-lookbehind rx)
  (unless (or (regexp? rx) (byte-regexp? rx))
    (raise-argument-error 'regexp-max-lookbehind "(or regexp? byte-regexp?)" rx))
  (rx:regexp-max-lookbehind rx))

;; ----------------------------------------

;; For especially simple and common cases, reduce the overhead created
;; by the general case by checking for simple cases and using a faster,
;; specific driver.

(define no-prefix #"")

(define (fast-bytes? rx in start-pos end-pos out prefix)
  (and (byte-regexp? rx)
       (bytes? in)
       (exact-nonnegative-integer? start-pos)
       (let ([len (bytes-length in)])
         (and (start-pos . <= . len)
              (or (not end-pos)
                  (and (exact-nonnegative-integer? end-pos)
                       (end-pos . <= . len)
                       (end-pos . >= . start-pos)))))
       (not out)
       (eq? prefix no-prefix)))

(define (fast-string? rx in start-pos end-pos out prefix)
  (and (regexp? rx)
       (string? in)
       (exact-nonnegative-integer? start-pos)
       (let ([len (string-length in)])
         (and (len . < . FAST-STRING-LEN)
              (start-pos . <= . len)
              (or (not end-pos)
                  (and (exact-nonnegative-integer? end-pos)
                       (end-pos . <= . len)
                       (end-pos . >= . start-pos)))))
       (not out)
       (eq? prefix no-prefix)))

(define (regexp-match? rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix])
  (cond
   [(fast-bytes? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match?/bytes rx in start-pos end-pos)]
   [(fast-string? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match?/string rx in start-pos end-pos)]
   [else
    (drive-regexp-match 'regexp-match? rx in start-pos end-pos out prefix
                        #:mode '?)]))

(define (regexp-match-positions rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix])
  (cond
   [(fast-bytes? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match-positions/bytes rx in start-pos end-pos)]
   [(fast-string? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match-positions/string rx in start-pos end-pos)]
   [else
    (drive-regexp-match 'regexp-match-positions rx in start-pos end-pos out prefix
                        #:mode 'positions)]))

(define (regexp-match rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix])
  (cond
   [(fast-bytes? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match/bytes rx in start-pos end-pos)]
   [(fast-string? rx in start-pos end-pos out prefix)
    (fast-drive-regexp-match/string rx in start-pos end-pos)]
   [else
    (drive-regexp-match 'regexp-match rx in start-pos end-pos out prefix
                        #:mode 'strings)]))

(define (regexp-match-positions/end rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match 'regexp-match-positions rx in start-pos end-pos out prefix
                      #:mode 'positions
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))

(define (regexp-match/end rx in [start-pos 0] [end-pos #f] [out #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match 'regexp-match rx in start-pos end-pos out prefix
                      #:mode 'strings
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))

(define (regexp-match-peek rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match 'regexp-match rx in start-pos end-pos #f prefix
                      #:peek? #t
                      #:progress-evt progress-evt
                      #:mode 'strings))

(define (regexp-match-peek-immediate rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match 'regexp-match rx in start-pos end-pos #f prefix
                      #:peek? #t #:immediate-only? #t
                      #:progress-evt progress-evt
                      #:mode 'strings))

(define (regexp-match-peek-positions rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match 'regexp-match rx in start-pos end-pos #f prefix
                      #:peek? #t
                      #:progress-evt progress-evt
                      #:mode 'positions))

(define (regexp-match-peek-positions/end rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match 'regexp-match rx in start-pos end-pos #f prefix
                      #:peek? #t
                      #:progress-evt progress-evt
                      #:mode 'positions
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))

(define (regexp-match-peek-positions-immediate rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix])
  (drive-regexp-match 'regexp-match rx in start-pos end-pos #f prefix
                      #:peek? #t #:immediate-only? #t
                      #:progress-evt progress-evt
                      #:mode 'positions))

(define (regexp-match-peek-positions-immediate/end rx in [start-pos 0] [end-pos #f] [progress-evt #f] [prefix no-prefix] [end-bytes-count 1])
  (drive-regexp-match 'regexp-match rx in start-pos end-pos #f prefix
                      #:peek? #t #:immediate-only? #t
                      #:progress-evt progress-evt
                      #:mode 'positions
                      #:end-bytes? #t
                      #:end-bytes-count end-bytes-count))
