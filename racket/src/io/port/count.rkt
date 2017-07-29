#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "../string/utf-8-decode.rkt")

(provide port-count-lines-enabled

         port-count-lines!
         port-counts-lines?
         port-next-location
         set-port-next-location!
         
         port-count!
         port-count-byte!)

(define port-count-lines-enabled
  (make-parameter #f (lambda (v) (and v #t))))

(define/who (port-count-lines! p)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [(output-port? p) (->core-output-port p)]
             [else (check who #:test #f #:contract "port?" p)])])
    (atomically
     (unless (core-port-count? p)
       (set-core-port-count?! p #t)
       (set-core-port-line! p 1)
       (set-core-port-column! p 0)
       (set-core-port-position! p (add1 (or (core-port-offset p) 0)))
       (define count-lines! (core-port-count-lines! p))
       (when count-lines!
         (end-atomic)
         (count-lines!)
         (start-atomic))))))

(define/who (port-counts-lines? p)
  (core-port-count?
   (cond
     [(input-port? p) (->core-input-port p)]
     [(output-port? p) (->core-output-port p)]
     [else
      (check who #:test #f #:contract "port?" p)])))

(define/who (port-next-location p)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [(output-port? p) (->core-output-port p)]
             [else
              (check who #:test #f #:contract "port?" p)])])
    (cond
      [(core-port-count? p)
       (define get-location (core-port-get-location p))
       (cond
         [get-location
          (get-location)]
         [else
          (values (core-port-line p)
                  (core-port-column p)
                  (core-port-position p))])]
      [(core-port-file-position p)
       => (lambda (file-position)
            (define offset (file-position))
            (values #f #f (and offset (add1 offset))))]
      [else
       (define offset (core-port-offset p))
       (values #f #f (and offset (add1 offset)))])))

(define/who (set-port-next-location! p line col pos)
  (check who (lambda (p) (or (input-port? p) (output-port? p)))
         #:contract "port?"
         p)
  (check who #:or-false exact-positive-integer? line)
  (check who #:or-false exact-nonnegative-integer? col)
  (check who #:or-false exact-positive-integer? pos)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [else (->core-output-port p)])])
    (atomically
     (when (and (core-port-count? p)
                (not (core-port-count-lines! p)))
       (set-core-port-line! p line)
       (set-core-port-column! p col)
       (set-core-port-position! p pos)))))

;; in atomic mode
;; When line counting is enabled, increment line, column, etc. counts,
;; which involves UTF-8 decoding
(define (port-count! in amt bstr start)
  (increment-offset! in amt)
  (when (core-port-count? in)
    (define end (+ start amt))
    (let loop ([i start]
               [span 0] ; number of previous bytes still to send to UTF-8 decoding
               [line (core-port-line in)]
               [column (core-port-column in)]
               [position (core-port-position in)]
               [state (core-port-state in)]
               [cr-state (core-port-cr-state in)]) ; #t => previous char was #\return
      (cond
       [(= i end)
        (cond
         [(zero? span)
          (set-core-port-line! in line)
          (set-core-port-column! in column)
          (set-core-port-position! in position)
          (set-core-port-state! in state)
          (set-core-port-cr-state! in cr-state)]
         [else
          ;; span doesn't include CR, LF, or tab
          (define-values (used-bytes got-chars new-state)
            (utf-8-decode! bstr (- end span) end
                           #f 0 #f
                           #:error-char #\?
                           #:abort-mode 'state
                           #:state state))
          (define (keep-aborts s) (if (eq? s 'complete) #f s))
          (loop end 0 line (and column (+ column got-chars)) (and position (+ position got-chars)) (keep-aborts new-state) #f)])]
       [else
        (define b (bytes-ref bstr i))
        (define (end-utf-8) ; => next byte is ASCII, so we can terminate a UTF-8 sequence
          (define-values (used-bytes got-chars new-state)
            (utf-8-decode! bstr (- i span) i
                           #f 0 #f
                           #:error-char #\?
                           #:state state))
          (loop end 0 line (and column (+ column got-chars)) (and position (+ position got-chars)) #f #f))
        (cond
         [(eq? b (char->integer #\newline))
          (cond
           [(or state (not (zero? span))) (end-utf-8)]
           [cr-state
            ;; "\r\n" combination counts as a single position
            (loop (add1 i) 0 line column position #f #f)]
           [else
            (loop (add1 i) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f #f)])]
         [(eq? b (char->integer #\return))
          (if (and (zero? span)(not state))
              (loop (add1 i) 0 (and line (add1 line)) (and column 0) (and position (add1 position)) #f #t)
              (end-utf-8))]
         [(eq? b (char->integer #\tab))
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (and column (+ column (bitwise-and (+ column 7) -8))) (and position (add1 position)) #f #f)
              (end-utf-8))]
         [(b . < . 128)
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (and column (add1 column)) (and position (add1 position)) #f #f)
              (loop (add1 i) (add1 span) line column position state #f))]
         [else
          (loop (add1 i) (add1 span) line column position state #f)])]))))

;; in atomic mode
;; If `b` is not a byte, it is treated like
;; a non-whitespace byte.
(define (port-count-byte! in b)
  (increment-offset! in 1)
  (when (core-port-count? in)
    (cond
     [(or (core-port-state in)
          (core-port-cr-state in)
          (eq? b (char->integer #\return))
          (eq? b (char->integer #\newline))
          (eq? b (char->integer #\tab)))
      (port-count! in 1 (bytes b) 0)]
     [else
      (let ([column (core-port-column in)]
            [position (core-port-position in)])
        (when position (set-core-port-position! in (add1 position)))
        (when column (set-core-port-column! in (add1 column))))])))

;; in atomic mode
(define (increment-offset! in amt)
  (define old-offset (core-port-offset in))
  (when old-offset
    (set-core-port-offset! in (+ amt old-offset))))
