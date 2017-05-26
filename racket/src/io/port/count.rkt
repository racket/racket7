#lang racket/base
(require "../common/check.rkt"
         "../common/atomic.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "../string/utf-8-decode.rkt")

(provide port-count-lines-enabled

         port-count-lines!
         port-counts-lines?
         port-next-location
         set-port-next-location!
         
         input-port-count!
         input-port-count-byte!)

(define port-count-lines-enabled
  (make-parameter #f (lambda (v) (and v #t))))

(define (port-count-lines! p)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (unless (core-input-port-line p)
         (set-core-input-port-line! p 1)
         (set-core-input-port-column! p 0)
         (set-core-input-port-position! p 1)
         (define count-lines! (core-input-port-count-lines! p))
         (when count-lines! (count-lines!))))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (unless (core-output-port-line p)
         (set-core-output-port-line! p 1)
         (set-core-output-port-column! p 0)
         (set-core-output-port-position! p 1)
         (define count-lines! (core-output-port-count-lines! p))
         (when count-lines! (count-lines!))))]
    [else
     (check 'port-count-lines! (lambda (x) #f)
            #:contract "port?"
            p)]))

(define (port-counts-lines? p)
  (cond
    [(input-port? p)
     (and (core-input-port-line (->core-input-port p)) #t)]
    [(output-port? p)
     (and (core-output-port-line (->core-output-port p)) #t)]
    [else
     (check 'port-counts-lines? (lambda (x) #f)
            #:contract "port?"
            p)]))

(define (port-next-location p)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (values (core-input-port-line p)
               (core-input-port-column p)
               (core-input-port-position p)))]
    [(output-port? p)
     (let ([p (->core-output-port p)])
       (values (core-output-port-line p)
               (core-output-port-column p)
               (core-output-port-position p)))]
    [else
     (check 'port-next-location (lambda (x) #f)
            #:contract "port?"
            p)]))

(define (set-port-next-location! p line col pos)
  (check 'set-port-next-location! (lambda (p) (or (input-port? p) (output-port? p)))
         #:contract "port?"
         p)
  (check 'set-port-next-location! (lambda (v) (or (not v) (exact-positive-integer? v)))
         #:contract "(or/c #f exact-positive-integer?)"
         line)
  (check 'set-port-next-location! (lambda (v) (or (not v) (exact-nonnegative-integer? v)))
         #:contract "(or/c #f exact-nonnegative-integer?)"
         col)
  (check 'set-port-next-location! (lambda (v) (or (not v) (exact-positive-integer? v)))
         #:contract "(or/c #f exact-positive-integer?)"
         pos)
  (cond
    [(input-port? p)
     (let ([p (->core-input-port p)])
       (atomically
        (when (and (core-input-port-line p)
                   (not (core-input-port-count-lines! p)))
          (set-core-input-port-line! p line)
          (set-core-input-port-column! p col)
          (set-core-input-port-position! p pos))))]
    [else
     (let ([p (->core-output-port p)])
       (atomically
        (when (and (core-output-port-line p)
                   (not (core-output-port-count-lines! p)))
          (set-core-output-port-line! p line)
          (set-core-output-port-column! p col)
          (set-core-output-port-position! p pos))))]))

;; When line counting is enabled, increment line, column, etc. counts,
;; which involves UTF-8 decoding
(define (input-port-count! in amt bstr start)
  (set-core-input-port-offset! in (+ amt (core-input-port-offset in)))
  (when (core-input-port-line in)
    (define end (+ start amt))
    (let loop ([i start]
               [span 0] ; number of previous bytes still to send to UTF-8 decoding
               [line (core-input-port-line in)]
               [column (core-input-port-column in)]
               [position (core-input-port-position in)]
               [state (core-input-port-state in)]
               [cr-state (core-input-port-cr-state in)]) ; #t => previous char was #\return
      (cond
       [(= i end)
        (cond
         [(zero? span)
          (set-core-input-port-line! in line)
          (set-core-input-port-column! in column)
          (set-core-input-port-position! in position)
          (set-core-input-port-state! in state)
          (set-core-input-port-cr-state! in cr-state)]
         [else
          ;; span doesn't include CR, LF, or tab
          (define-values (used-bytes got-chars new-state)
            (utf-8-decode! bstr (- end span) end
                           #f 0 #f
                           #:error-char #\?
                           #:abort-mode 'state
                           #:state state))
          (define (keep-aborts s) (if (eq? s 'complete) #f s))
          (loop end 0 line (+ column got-chars) (+ position got-chars) (keep-aborts new-state) #f)])]
       [else
        (define b (bytes-ref bstr i))
        (define (end-utf-8) ; => next byte is ASCII, so we can terminate a UTF-8 sequence
          (define-values (used-bytes got-chars new-state)
            (utf-8-decode! bstr (- i span) i
                           #f 0 #f
                           #:error-char #\?
                           #:state state))
          (loop end 0 line (+ column got-chars) (+ position got-chars) #f #f))
        (cond
         [(eq? b (char->integer #\newline))
          (cond
           [(or state (not (zero? span))) (end-utf-8)]
           [cr-state
            (loop (add1 i) 0 line column (add1 position) #f #f)]
           [else
            (loop (add1 i) 0 (add1 line) 0 (add1 position) #f #f)])]
         [(eq? b (char->integer #\return))
          (if (and (zero? span)(not state))
              (loop (add1 i) 0 (add1 line) 0 (add1 position) #f #t)
              (end-utf-8))]
         [(eq? b (char->integer #\tab))
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (+ column (bitwise-and (+ column 7) -8)) (add1 position) #f #f)
              (end-utf-8))]
         [(b . < . 128)
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (add1 column) (add1 position) #f #f)
              (loop (add1 i) (add1 span) line column position state #f))]
         [else
          (loop (add1 i) (add1 span) line column position state #f)])]))))
  
(define (input-port-count-byte! in b)
  (set-core-input-port-offset! in (add1 (core-input-port-offset in)))
  (when (core-input-port-line in)
    (cond
     [(or (core-input-port-state in)
          (core-input-port-cr-state in)
          (eq? b (char->integer #\return))
          (eq? b (char->integer #\newline))
          (eq? b (char->integer #\tab)))
      (input-port-count! in 1 (bytes b) 0)]
     [else
      (set-core-input-port-position! in (add1 (core-input-port-position in)))
      (set-core-input-port-column! in (add1 (core-input-port-column in)))])))
